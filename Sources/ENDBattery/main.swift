// swift-min-version: 5.2

/* ————————————————————————————— 需要认真填的数据 ————————————————————————————— */

extension Config {
	static let goody = Config(
		/// 固定消耗的电池: .originium, .green, .blue, .purple, .lowEarth
		staticBattery: .purple,
		
		/// 需要分流的电池: .originium, .green, .blue, .purple, .lowEarth
		analyzedBattery: .purple,
		
		/// 你屏幕上显示的总功率需求
		baseRequiredPower: 4775
	)
	
	static let wuling = Config(
		/// 固定消耗的电池: .originium, .green, .blue, .purple, .lowEarth
		staticBattery: .lowEarth,
		
		/// 需要分流的电池: .originium, .green, .blue, .purple, .lowEarth
		analyzedBattery: .lowEarth,
		
		/// 你屏幕上显示的总功率需求
		baseRequiredPower: 1885
	)
}

/// 启用的配置
let config: Config = .goody

// 额外的传送带/分流器数量（格），用于补偿分析电池数量增加带来的分流效率下降
let extraBelt: Int = 2

/* ————————————————————————————— 选填的数据 ————————————————————————————— */

/// 最终输出的方案数量, 1会输出最优方案, 3会输出前三方案, 以此类推
let showTopSolutions = 1

/// 需要分流的电池数量, 一般不用改
let analyzedBatteryCount = 1

/// 发电机数量, 一般不用改
let generatorCount = analyzedBatteryCount

// 是否允许使用三分流（增加搜索空间和时间, 但可能找到更优解）
let enableThree = true

/* ————————————————————————————— 下面不用看 ————————————————————————————— */

import Foundation

let startTime = Date()

defer {
	let elapsed = Date().timeIntervalSince(startTime)
	print(String(format: "\n总耗时: %.2f 秒", elapsed))
}

struct Config {
	// --- Game Rules & Constants (游戏规则与常量) ---
	// Batteries can only occupy one generator at a time to generate electricity
	// Each battery lasts for 40 seconds
	// Conveyor belt speed is 2 seconds per grid, splitter is the same
	
	enum Battery {
		case originium
		case green
		case blue
		case purple
		case lowEarth
		
		var power: Double {
			switch self {
			case .originium:
				return 50
			case .green:
				return 220
			case .blue:
				return 420
			case .purple:
				return 1100
			case .lowEarth:
				return 1600
			}
		}
		
		var life: Double {
			switch self {
			case .originium:
				return 8
			case .green, .blue, .purple, .lowEarth:
				return 40
			}
		}
		
		var totalEnergy: Double {
			power * life
		}
		
		var description: String {
			switch self {
			case .originium:
				return "源石"
			case .green:
				return "绿电池"
			case .blue:
				return "蓝电池"
			case .purple:
				return "紫电池"
			case .lowEarth:
				return "低容息壤电池"
			}
		}
	}
	
	let staticBattery: Battery
	let analyzedBattery: Battery
	let baseRequiredPower: Double
}

// 目标总功率需求 (瓦特), 核心自发电200
var requiredPower: Double = config.baseRequiredPower - 200

// --- Derived Variables (衍生变量) ---
let batteryStatic = config.staticBattery
let battery = config.analyzedBattery

let currentDelay = Double(extraBelt) * 2.0
let efficiency = max(0, battery.life / (battery.life + currentDelay))
print("分流效率: \(efficiency)")

let preSplit: Double = battery == .originium ? pow(2, 2) : pow(3, 2) // Hardcoded to discard the first n k-way splits. Originium power is too low, so only binary split twice.
let depthLimit = Int(preSplit) + 1
print("搜索深度: \(depthLimit - 1)")

var totalBatteryPower: Double = battery.totalEnergy / 2 / preSplit
let staticBatteryCount: Int = Int(requiredPower / batteryStatic.power)
requiredPower -= batteryStatic.power * Double(staticBatteryCount)

let batteryCount = Int(requiredPower / battery.power) + 1

if batteryCount > analyzedBatteryCount {
	requiredPower -= battery.power * Double(batteryCount - analyzedBatteryCount)
}

guard battery.power * Double(analyzedBatteryCount) > requiredPower else {
	print("\(analyzedBatteryCount)个\(battery.description)(\(battery.power * Double(analyzedBatteryCount))w)无法满足需求(\(requiredPower)w)，请增加分析的电池数量")
	exit(0)
}

print("可用电池功率: \(totalBatteryPower)")
print("所需功率: \(requiredPower)")
print("所需电池数量: \(batteryStatic.description): \(staticBatteryCount)个, \(battery.description): \(analyzedBatteryCount)个")

let oneBatteryTotalEnergyGlob = battery.totalEnergy
let inputDoubleBatteryPowerGlob = battery.power * Double(analyzedBatteryCount)
let baselineBatteriesPerDayGlob = (inputDoubleBatteryPowerGlob * 86400.0) / oneBatteryTotalEnergyGlob
let requiredBatteriesPerDayGlob = (requiredPower * 86400.0) / oneBatteryTotalEnergyGlob

print("基准消耗: \(String(format: "%.3f", baselineBatteriesPerDayGlob)) 颗/天 (\(analyzedBatteryCount)发电机常开)")
print("理论最少: \(String(format: "%.3f", requiredBatteriesPerDayGlob)) 颗/天 (100%利用率)")
print("理论可省: \(String(format: "%.3f", baselineBatteriesPerDayGlob - requiredBatteriesPerDayGlob)) 颗/天")

enum SplitType {
	case two
	case three
}

enum Action {
	case add
	case discard
}

struct Step: Equatable {
	let type: SplitType
	let action: Action
}

struct OverlapProfile: Comparable, CustomStringConvertible {
	let overflowPerSecond: Double
	let minBatteryLevel: Double
	let netBenefitPerSecond: Double // Net savings in batteries per second
	let outageDurationPer1000Sec: Double // Average outage duration per 1000 seconds
	
	var description: String {
		return "净收益: \(String(format: "%.6f", netBenefitPerSecond)) 颗/秒, 溢出/秒: \(String(format: "%.2f", overflowPerSecond)), 最低电量: \(String(format: "%.2f", minBatteryLevel)), 停电: \(String(format: "%.3f", outageDurationPer1000Sec))秒/1000秒"
	}
	
	static func < (lhs: OverlapProfile, rhs: OverlapProfile) -> Bool {
		if abs(lhs.outageDurationPer1000Sec - rhs.outageDurationPer1000Sec) > 1e-9 {
			return lhs.outageDurationPer1000Sec < rhs.outageDurationPer1000Sec // Lower outage is better
		}
		if abs(lhs.netBenefitPerSecond - rhs.netBenefitPerSecond) > 1e-9 {
			return lhs.netBenefitPerSecond > rhs.netBenefitPerSecond // Higher benefit is better
		}
		
		if abs(lhs.overflowPerSecond - rhs.overflowPerSecond) > 1e-9 {
			return lhs.overflowPerSecond < rhs.overflowPerSecond
		}
		return lhs.minBatteryLevel > rhs.minBatteryLevel
	}
}

// Model to hold a found solution
struct Solution {
	let finalC: Double
	let entropy: Double
	let steps: [Step]
	let splitValues: [Double]
	let diff: Double
	let overlap: OverlapProfile
}

struct OverlapStats {
	let cycleTime: Double
	let overflow: Double
	let minLevel: Double
	let profile: OverlapProfile
}

func getOverlapStats(steps: [Step]) -> OverlapStats {
	let rootPeriod: Int = 9
	struct Stream {
		let period: Int
		let offset: Int
	}
	var activeStreams: [Stream] = []
	
	var currentP = rootPeriod
	var currentO = 0
	
	for step in steps {
		let isTernary = (step.type == .three)
		let multiplier = isTernary ? 3 : 2
		
		let branchP = currentP * multiplier
		let branchO = currentO
		
		let nextP = currentP * multiplier
		let nextO = currentO + currentP
		
		if step.action == .add {
			activeStreams.append(Stream(period: branchP, offset: branchO))
		}
		
		currentP = nextP
		currentO = nextO
	}
	
	func gcd(_ a: Int, _ b: Int) -> Int {
		let r = a % b
		return r == 0 ? b : gcd(b, r)
	}
	func lcm(_ a: Int, _ b: Int) -> Int {
		return (a * b) / gcd(a, b)
	}
	
	var cycle = rootPeriod
	if activeStreams.isEmpty {
		// Default small cycle if no streams
		cycle = rootPeriod
	} else {
		for s in activeStreams {
			cycle = lcm(cycle, s.period)
		}
	}
	
	// Simulation duration: 2 cycles.
	// We simulate from t=0 to establish the correct battery level state,
	// then measure stats only during the 2nd cycle (cycleTime to 2*cycleTime).
	let singleCycleDuration = Double(cycle) * 2.0
	let cycleTime = singleCycleDuration // maintain variable name compatibility
	
	// Generate events
	var offsets: [Int] = []
	for s in activeStreams {
		let startPhase = s.offset % cycle
		var t = startPhase
		while t < cycle {
			offsets.append(t)
			t += s.period
		}
	}
	offsets.sort()
	
	let baseArrivals = offsets.map {
		Double($0) * 2.0
	}
	
	// Constants for Net Benefit Calculation
	let oneBatteryTotalEnergy = battery.totalEnergy
	let excessPowerWithoutSplit = (battery.power * Double(analyzedBatteryCount)) - requiredPower
	let saveRateInBatteriesPerSec = (excessPowerWithoutSplit > 0.001) ? (excessPowerWithoutSplit / oneBatteryTotalEnergy) : 0.0
	
	if baseArrivals.isEmpty {
		return OverlapStats(cycleTime: cycleTime, overflow: 0.0, minLevel: 100000.0, profile: OverlapProfile(overflowPerSecond: 0.0, minBatteryLevel: 100000.0, netBenefitPerSecond: saveRateInBatteriesPerSec, outageDurationPer1000Sec: 0.0))
	}
	
	// Simulate 2 cycles
	var allArrivals: [Double] = []
	for t in baseArrivals {
		allArrivals.append(t)
	}
	for t in baseArrivals {
		allArrivals.append(t + singleCycleDuration)
	}
	
	// Distribute to Generators Alternately (Round Robin)
	var genArrivals: [[Double]] = Array(repeating: [], count: generatorCount)
	
	for (i, t) in allArrivals.enumerated() {
		let index = i % generatorCount
		genArrivals[index].append(t)
	}
	
	struct Interval {
		let start: Double
		let end: Double
	}
	
	func getIntervals(arrivals: [Double]) -> [Interval] {
		var intervals: [Interval] = []
		var nextAvailable: Double = 0.0
		// Queue logic: Battery waits if generator is busy
		for t in arrivals {
			let start = max(t, nextAvailable)
			let end = start + battery.life
			intervals.append(Interval(start: start, end: end))
			nextAvailable = end
		}
		return intervals
	}
	
	var combinedIntervals: [Interval] = []
	for arrivals in genArrivals {
		combinedIntervals.append(contentsOf: getIntervals(arrivals: arrivals))
	}
	
	// Create Event Points for the entire simulation timeline
	struct EventPoint {
		let time: Double
		let type: Int
	}
	var pevents: [EventPoint] = []
	
	for inv in combinedIntervals {
		pevents.append(EventPoint(time: inv.start, type: 1))
		pevents.append(EventPoint(time: inv.end, type: -1))
	}
	
	pevents.sort { a, b in
		// If times are equal, process end events (-1) before start events (1)
		if abs(a.time - b.time) < 0.0001 {
			return a.type < b.type
		}
		return a.time < b.time
	}
	
	// Measurement Window: [cycleTime, 2*cycleTime]
	let measureStart = singleCycleDuration
	let measureEnd = 2.0 * singleCycleDuration
	let measurementDuration = measureEnd - measureStart
	
	var active = 0
	var lastT = 0.0
	var currentLevel = 100000.0 // Correctly start full at t=0
	let maxLevel = 100000.0
	
	// Stats accumulators (only applied within measurement window)
	var totalOverflow = 0.0
	var totalOutageTime = 0.0
	var minLevel = 100000.0
	
	// To properly track minLevel within the window, we should initialize it to currentLevel
	// when we first cross into the measurement window, or just track it globally and
	// realize that if it drops low in cycle 1, it matters.
	// However, the requirement is "calculate ... in a period".
	// Usually this means steady state. We will track minLevel only inside the window.
	var minLevelInitialized = false
	
	for e in pevents {
		let t = e.time
		let dt = t - lastT
		
		if dt > 0.00001 {
			// Integrate from lastT to t
			let inputPower = Double(active) * battery.power
			let netPower = inputPower - requiredPower // requiredPower accounts for base generator (200W)
			
			// Determine intersection with measurement window
			let segStart = lastT
			let segEnd = t
			let overlapStart = max(segStart, measureStart)
			let overlapEnd = min(segEnd, measureEnd)
			let overlapDt = overlapEnd - overlapStart
			
			// Evolve state step by step (linearly)
			// But we can just calculate the end state.
			// CAUTION: Level is clamped at Bounds (0, 100000).
			// We must simulate the level evolution over the full dt to get accurate currentLevel at t.
			
			let levelBefore = currentLevel
			let possibleLevel = currentLevel + netPower * dt
			
			// Handle clamping and stats
			var nextLevel = possibleLevel
			
			if possibleLevel > maxLevel {
				nextLevel = maxLevel
			} else if possibleLevel <= 0 {
				nextLevel = 0
			}
			
			// Now distribute stats to measurement window if overlapping
			if overlapDt > 0.000001 {
				// We need to know explicitly correctly what happened during the overlap window.
				// Re-calculating logic just for the overlap window is safer.
				
				// State at start of overlap window:
				// We can linearly interpolate currentLevel at overlapStart,
				// BUT we need to be careful if we hit limits before overlapStart.
				// Actually, since we update currentLevel sequentially,
				// we should just run the simulation logic for the specific segment.
				// But the segment [lastT, t] has CONSTANT active power.
				
				// Let's refine: The change is linear/clamped within this segment.
				// We update currentLevel fully for the segment at the end.
				// But we accumulate stats proportionally? No, clamping is non-linear.
				
				// Correct approach:
				// 1. Calculate state at overlapStart (if lastT < measureStart)
				// 2. Calculate state evolution from overlapStart to overlapEnd
				
				// Case A: Segment is fully before window -> handled by main loop update, no stats.
				// Case B: Segment is fully after window -> loop breaks/waste.
				// Case C: Overlap.
				
				// Let's calculate the effective contributions.
				
				// Project level at overlapStart
				let dtPre = max(0, overlapStart - lastT)
				var levelAtWindowEntry = levelBefore
				if dtPre > 0 {
					let p = levelBefore + netPower * dtPre
					levelAtWindowEntry = min(maxLevel, max(0, p))
				}
				
				// Now simulate from levelAtWindowEntry over overlapDt
				let pEnd = levelAtWindowEntry + netPower * overlapDt
				
				if pEnd > maxLevel {
					totalOverflow += (pEnd - maxLevel)
				}
				
				if pEnd <= 0 {
					let timeToZeroLocal = (netPower < -0.00001) ? (levelAtWindowEntry / abs(netPower)) : 0.0
					let outageLocal = max(0, overlapDt - timeToZeroLocal)
					totalOutageTime += outageLocal
				}
				
				// Update minLevel strictly using values seen INSIDE the window
				if !minLevelInitialized {
					minLevel = levelAtWindowEntry
					minLevelInitialized = true
				} else {
					minLevel = min(minLevel, levelAtWindowEntry)
				}
				// Also check end of window segment (clamped)
				let levelAtWindowExit = min(maxLevel, max(0, pEnd))
				minLevel = min(minLevel, levelAtWindowExit)
			}
			
			// Apply full segment update to state for next iteration
			currentLevel = nextLevel
		}
		
		active += e.type
		lastT = t
		
		if lastT >= measureEnd {
			break
		}
	}
	
	// Handle tail if any (lastT < measureEnd)
	// Though with sorted events and forced window, we usually cover it.
	// If the last event was before measureEnd, we must simulate until measureEnd.
	if lastT < measureEnd {
		let dt = measureEnd - lastT
		if dt > 0.00001 {
			// Within window
			if !minLevelInitialized {
				minLevel = currentLevel
				minLevelInitialized = true
			}
			
			let inputPower = Double(active) * battery.power
			let netPower = inputPower - requiredPower
			
			let possibleLevel = currentLevel + netPower * dt
			
			if possibleLevel > maxLevel {
				totalOverflow += (possibleLevel - maxLevel)
				currentLevel = maxLevel // visualization state
			} else if possibleLevel <= 0 {
				let timeToZero = (netPower < -0.00001) ? (currentLevel / abs(netPower)) : 0.0
				let outage = max(0, dt - timeToZero)
				totalOutageTime += outage
				currentLevel = 0
			} else {
				currentLevel = possibleLevel
			}
			minLevel = min(minLevel, currentLevel)
		}
	}
	
	// Fallback if window was completely empty of events (should be rare/impossible unless cycle=0)
	if !minLevelInitialized {
		minLevel = currentLevel
	}
	
	let overflowPerSec = totalOverflow / measurementDuration
	let wasteRateInBatteriesPerSec = (overflowPerSec > 0.000001) ? (overflowPerSec / oneBatteryTotalEnergy) : 0.0
	let netBenefitPerSec = saveRateInBatteriesPerSec - wasteRateInBatteriesPerSec
	
	let outageDurationPer1000Sec = (measurementDuration > 0.001) ? ((totalOutageTime / measurementDuration) * 1000.0) : 0.0
	
	let profile = OverlapProfile(overflowPerSecond: overflowPerSec, minBatteryLevel: minLevel, netBenefitPerSecond: netBenefitPerSec, outageDurationPer1000Sec: outageDurationPer1000Sec)
	
	return OverlapStats(
		cycleTime: measurementDuration,
		overflow: totalOverflow,
		minLevel: minLevel,
		profile: profile
	)
}

func calculateOverlapProfile(steps: [Step]) -> OverlapProfile {
	return getOverlapStats(steps: steps).profile
}

var solutions: [Solution] = []

// Recursive Search Function
// Pass 'maxSteps' for pruning
func binarySplit(sourceVal: Double, testBattery: Double, entropy: Double, n: Int, steps: [Step], values: [Double], maxSteps: Int, threeCount: Int, solutions: inout [Int: Solution]) {
	
	guard
		sourceVal >= 1.0,
		steps.count < maxSteps,
		testBattery + sourceVal > requiredPower
	else {
		return
	}
	
	// Capture valid solution
	let diff = testBattery - requiredPower
	if diff >= 0 && diff <= 50 {
		let profile = calculateOverlapProfile(steps: steps)
		
		let sol = Solution(
			finalC: testBattery,
			entropy: entropy,
			steps: steps,
			splitValues: values,
			diff: diff,
			overlap: profile
		)
		
		let len = steps.count
		if let existing = solutions[len] {
			if isBetterSolution(sol, than: existing) {
				solutions[len] = sol
			}
		} else {
			solutions[len] = sol
		}
	}
	
	// Perform Binary Split
	
	let half = sourceVal / 2.0
	
	// Branch 1: Add to C
	binarySplit(
		sourceVal: half,
		testBattery: testBattery + (half * efficiency),
		entropy: entropy,
		n: n + 1,
		steps: steps + [Step(type: .two, action: .add)],
		values: values + [half],
		maxSteps: maxSteps,
		threeCount: threeCount,
		solutions: &solutions
	)
	
	// Branch 2: Discard
	// Entropy increases by 1/(2^n).
	let entropyIncrement = 1.0 / pow(2.0, Double(n))
	binarySplit(
		sourceVal: half,
		testBattery: testBattery,
		entropy: entropy + entropyIncrement,
		n: n + 1,
		steps: steps + [Step(type: .two, action: .discard)],
		values: values + [half],
		maxSteps: maxSteps,
		threeCount: threeCount,
		solutions: &solutions
	)
	
	guard enableThree else {
		return
	}
	
	// Perform Ternary Split
	// 1/3 to garbage (implicit), 1/3 to next recursion, 1/3 to process
	let third = sourceVal / 3.0
	
	// Branch 3: Ternary Add
	// entropy unchanged, n unchanged, actions/values unchanged
	binarySplit(
		sourceVal: third,
		testBattery: testBattery + (third * efficiency),
		entropy: entropy,
		n: n,
		steps: steps + [Step(type: .three, action: .add)],
		values: values + [third],
		maxSteps: maxSteps,
		threeCount: threeCount + 1,
		solutions: &solutions
	)
	
	// Branch 4: Ternary Discard
	binarySplit(
		sourceVal: third,
		testBattery: testBattery,
		entropy: entropy,
		n: n,
		steps: steps + [Step(type: .three, action: .discard)],
		values: values + [third],
		maxSteps: maxSteps,
		threeCount: threeCount + 1,
		solutions: &solutions
	)
}

print("正在搜索...")

func isBetterSolution(_ new: Solution, than old: Solution?) -> Bool {
	guard let old = old else {
		return true
	}
	
	// Priority 1: Improve Overlap Profile
	if new.overlap != old.overlap {
		return new.overlap < old.overlap // Use Comparable implementation
	}
	
	// Priority 2: Improve Steps
	if new.steps.count != old.steps.count {
		return new.steps.count < old.steps.count
	}
	
	// Priority 3: Smaller Diff
	if abs(new.diff - old.diff) > 0.0001 {
		return new.diff < old.diff
	}
	
	// Priority 4: Higher Entropy
	return new.entropy > old.entropy
}

// Format helpers
func formatDuration(_ seconds: Double) -> String {
	if seconds.isInfinite { return "∞" }
	
	if seconds >= 86400 {
		return String(format: "%.3f 天", seconds/86400.0)
	}
	if seconds >= 3600 {
		return String(format: "%.3f 小时", seconds/3600.0)
	}
	if seconds >= 60 {
		return String(format: "%.3f 分钟", seconds/60.0)
	}
	return String(format: "%.3f 秒", seconds)
}

// --- Analysis Helper ---
func analyzeSolutionOverlap(_ solution: Solution) {
	let stats = getOverlapStats(steps: solution.steps)
	
	let actions = solution.steps.map { step -> String in
		let typeStr = (step.type == .two) ? "2" : "3"
		let actStr = (step.action == .add) ? "加" : "弃"
		return "\(typeStr)\(actStr)"
	}.joined(separator: "  ")
	
	// Calculate savePerDay again for display
	let netBenefit = stats.profile.netBenefitPerSecond
	let savedPerDay = (netBenefit > 0) ? (netBenefit * 86400.0) : 0.0
	
	print("\n==================================================")
	print("操作步骤: \(actions)")
	print("--- [步骤数: \(solution.steps.count)] (净收益: \(String(format: "%.6f", netBenefit)) 颗/秒, 每天: \(String(format: "%.3f", savedPerDay))颗) ---")
	
	print("周期: \(String(format: "%.3f", stats.cycleTime))秒 | 溢出/秒: \(String(format: "%.3f", stats.profile.overflowPerSecond)) | 最低电量: \(String(format: "%.2f", stats.minLevel))")
	if stats.minLevel < 0.001 {
		let outageStr = String(format: "%.3f", stats.profile.outageDurationPer1000Sec)
		print("⚠️ 警告: 该方案可能会导致短暂停电 (最低电量归零), 平均停电: \(outageStr)秒/1000秒 ⚠️")
	}
	print("最终功率: \(String(format: "%.4f", solution.finalC)) | 差值: \(String(format: "%.4f", solution.diff))")
	
	let oneBatteryTotalEnergy = battery.totalEnergy
	let inputBatteryPower = battery.power * Double(analyzedBatteryCount)
	
	// 1. Calculate time to waste 1 battery due to overflow
	let overflowPerSecond = stats.profile.overflowPerSecond
	let secondsToWasteOneBattery: Double = (overflowPerSecond > 0.001) ? (oneBatteryTotalEnergy / overflowPerSecond) : Double.infinity
	
	// 2. Calculate time to save 1 battery
	let excessPowerWithoutSplit = inputBatteryPower - requiredPower
	let secondsToSaveOneBattery = (excessPowerWithoutSplit > 0.001) ? (oneBatteryTotalEnergy / excessPowerWithoutSplit) : Double.infinity
	
	// 3. Net Benefit
	var netSecondsPerBattery: Double = 0.0
	
	let baselineBatteriesPerDay = (inputBatteryPower * 86400.0) / oneBatteryTotalEnergy
	
	print("理论每: \(formatDuration(secondsToSaveOneBattery)) 省一颗电池 (基准 \(analyzedBatteryCount)发电机满载)")
	print("实际每: \(formatDuration(secondsToWasteOneBattery)) 浪费一颗电池 (溢出)")
	
	if !secondsToSaveOneBattery.isInfinite {
		let saveRate = 1.0 / secondsToSaveOneBattery
		let wasteRate = (secondsToWasteOneBattery.isInfinite) ? 0.0 : (1.0 / secondsToWasteOneBattery)
		let netRate = saveRate - wasteRate
		
		let dailyLimit = abs(netRate) * 86400.0
		let actualConsumption = baselineBatteriesPerDay - dailyLimit // Approximate if saving
		
		if netRate > 0 {
			netSecondsPerBattery = 1.0 / netRate
			print("计算消耗: \(String(format: "%.3f", actualConsumption)) 颗/天")
			print("结论: 净收益 每天 省 \(String(format: "%.3f", dailyLimit)) 颗电池 (每 \(formatDuration(netSecondsPerBattery)) 一颗)")
		} else if netRate < 0 {
			netSecondsPerBattery = 1.0 / abs(netRate)
			print("结论: 净亏损 每天 亏 \(String(format: "%.3f", dailyLimit)) 颗电池 (每 \(formatDuration(netSecondsPerBattery)) 一颗)")
		} else {
			print("结论: 收支平衡")
		}
	} else {
		print("结论: 无法计算节省 (需求 >= \(analyzedBatteryCount)发电机?)")
	}
}

var allSolutionsMap: [Int: Solution] = [:]

// Single complete recursion with max depth
binarySplit(
	sourceVal: totalBatteryPower,
	testBattery: 0.0,
	entropy: 0.0,
	n: 0,
	steps: [],
	values: [],
	maxSteps: depthLimit,
	threeCount: 0,
	solutions: &allSolutionsMap
)

// Convert map to sorted list
let tops = allSolutionsMap.values.sorted(by: { (a, b) -> Bool in
	return isBetterSolution(a, than: b)
}).prefix(showTopSolutions)

for sol in tops {
	analyzeSolutionOverlap(sol)
}
