// swift-min-version: 5.2

/* ————————————————————————————— 需要认真填的数据 ————————————————————————————— */

let configs: [Config] = [
	Config(
		name: "4号谷地",
		/// 固定消耗的电池: .originium, .green, .blue, .purple, .lowEarth
		staticBattery: .purple,

		/// 需要分流的电池: .originium, .green, .blue, .purple, .lowEarth
		analyzedBattery: .purple,

		/// 你屏幕上显示的总功率需求
		baseRequiredPower: 4775
	),

	Config(
		name: "武陵",
		/// 固定消耗的电池: .originium, .green, .blue, .purple, .lowEarth
		staticBattery: .lowEarth,

		/// 需要分流的电池: .originium, .green, .blue, .purple, .lowEarth
		analyzedBattery: .lowEarth,

		/// 你屏幕上显示的总功率需求
		baseRequiredPower: 1885
	),
]

/* ————————————————————————————— 选填的数据 ————————————————————————————— */

/// 递归时额外的传送带/分流器数量（格）, 一般不用改
/// 用于限制递归上限
/// 最小值为 1
let extraBeltInSteps: Int = 1

/// 最大递归深度
let maxDepthLimit: Int = 9

/// 最终输出的方案数量, 1会输出最优方案, 3会输出前三方案, 以此类推
var showTopSolutions: Int = 3

/// 允许最小差值, 这个值越小越接近理论最优, 但是会下线后因为鹰角的服务器优化而导致计算不正确, 如果出现这种情况可以适当调大这个值到10以上
var allowedMinDiff: Double = 1

/// 是否允许使用三分流（增加搜索空间和时间, 但可能找到更优解）
let enableThree: Bool = true

/// 安全阈值: 如果电量曾经低于这个值, 方案会被严重降级. 这个值越高越安全, 但可能错过一些边缘方案. 建议设置在15%左右 (15000), 也可以根据需求调整.
let safetyThreshold = 0.15

/* ————————————————————————————— 下面不用看 ————————————————————————————— */

import Foundation

let startTime = Date()

defer {
	let elapsed = Date().timeIntervalSince(startTime)
	print(String(format: "\n🕐 总耗时: %.2f 秒", elapsed))
}

/// Core capacity.
let coreMaxCapacity: Double = 100000

struct Config {
	let name: String
	/// Game constants: Battery usage logic, lifespan, and transport speed.
	/// Batteries occupy generator one at a time.
	/// Belt speed: 2s/grid.

	enum Battery {
		case originium
		case green
		case blue
		case purple
		case lowEarth

		var name: String {
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
	}

	let staticBattery: Battery
	let analyzedBattery: Battery
	let baseRequiredPower: Double
}

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

struct OverlapProfile: Comparable {
	let overflowPerSecond: Double
	let minBatteryLevel: Double
	let endBatteryLevel: Double
	let hitFullCharge: Bool

	/// Net batteries saved per second
	let netBenefitPerSecond: Double

	/// Avg outage duration per 1000s
	let outageDurationPer1000Sec: Double

	static func < (lhs: OverlapProfile, rhs: OverlapProfile) -> Bool {
		if abs(lhs.outageDurationPer1000Sec - rhs.outageDurationPer1000Sec) > 1e-4 {
			// Min outage
			return lhs.outageDurationPer1000Sec < rhs.outageDurationPer1000Sec
		}

		// Prioritize sustainability (full recharge implies no long-term drain).
		if lhs.hitFullCharge != rhs.hitFullCharge {
			return lhs.hitFullCharge
		}

		// Safety check: Avoid low battery levels.
		let lhsLow = lhs.minBatteryLevel < coreMaxCapacity * safetyThreshold
		let rhsLow = rhs.minBatteryLevel < coreMaxCapacity * safetyThreshold
		if lhsLow || rhsLow {
			if abs(lhs.minBatteryLevel - rhs.minBatteryLevel) > 1e-4 {
				return lhs.minBatteryLevel > rhs.minBatteryLevel
			}
		}

		if abs(lhs.netBenefitPerSecond - rhs.netBenefitPerSecond) > 1e-4 {
			// Max benefit
			return lhs.netBenefitPerSecond > rhs.netBenefitPerSecond
		}

		if abs(lhs.overflowPerSecond - rhs.overflowPerSecond) > 1e-4 {
			return lhs.overflowPerSecond < rhs.overflowPerSecond
		}
		return lhs.minBatteryLevel > rhs.minBatteryLevel
	}
}

// Holds a valid solution
class Solution {
	let finalC: Double
	let entropy: Double
	var preSplitBits: [Int: Int]
	var steps: [Step]
	var splitValues: [Double]
	let diff: Double
	let overlap: OverlapProfile
	let actualBatteryCount: Int
	let requiredPower: Double
	let analyzedBatteryCount: Int
	let staticBatteryCount: Int
	let depthLimit: Int

	init(
		finalC: Double, entropy: Double, preSplitBits: [Int: Int], steps: [Step],
		splitValues: [Double], diff: Double, overlap: OverlapProfile, actualBatteryCount: Int,
		requiredPower: Double, analyzedBatteryCount: Int, staticBatteryCount: Int, depthLimit: Int
	) {
		self.finalC = finalC
		self.entropy = entropy
		self.preSplitBits = preSplitBits
		self.steps = steps
		self.splitValues = splitValues
		self.diff = diff
		self.overlap = overlap
		self.actualBatteryCount = actualBatteryCount
		self.requiredPower = requiredPower
		self.analyzedBatteryCount = analyzedBatteryCount
		self.staticBatteryCount = staticBatteryCount
		self.depthLimit = depthLimit
	}

	lazy var allActions: String = {
		// 1. Collect all actions including pre-split
		var allSteps: [(type: Int, action: Action)] = []

		// Pre-split bits are always discards
		for (bit, count) in preSplitBits {
			for _ in 0..<count {
				allSteps.append((type: bit, action: .discard))
			}
		}

		// Solution steps
		for step in steps {
			let bit = (step.type == .two) ? 2 : 3
			allSteps.append((type: bit, action: step.action))
		}

		var groups: [[(type: Int, action: Action)]] = []
		if !allSteps.isEmpty {
			var currentGroup: [(type: Int, action: Action)] = [allSteps[0]]
			for i in 1..<allSteps.count {
				let step = allSteps[i]
				let prev = allSteps[i - 1]
				if step.action == prev.action {
					currentGroup.append(step)
				} else {
					groups.append(currentGroup)
					currentGroup = [step]
				}
			}
			groups.append(currentGroup)
		}

		var resultParts: [String] = []

		for group in groups {
			let action = group[0].action
			var stepsToProcess = group

			// Sort only if it's a discard group
			if action == .discard {
				stepsToProcess.sort { $0.type > $1.type }  // 3 before 2
			}

			// Convert to strings with separators
			var groupStrings: [String] = []
			var lastType: Int? = nil

			for step in stepsToProcess {
				let typeStr = (step.type == 2) ? "2" : "3"
				let actStr = (step.action == .add) ? "🟢" : "🔴"
				let str = "\(typeStr)\(actStr)"

				if let last = lastType, last != step.type {
					groupStrings.append("»»» ")  // Inner group separator
				}
				groupStrings.append(str)
				lastType = step.type
			}
			resultParts.append(groupStrings.joined(separator: "  "))
		}

		return resultParts.joined(separator: "  »»»  ")
	}()
}

func isBetterSolution(_ new: Solution, than old: Solution?) -> Bool {
	guard let old = old else {
		return true
	}

	// Priorities: 1. Overlap Profile, 2. Fewer Steps, 3. Smaller Diff, 4. Higher Entropy
	if new.overlap != old.overlap {
		return new.overlap < old.overlap
	}

	if new.steps.count != old.steps.count {
		return new.steps.count < old.steps.count
	}

	if abs(new.diff - old.diff) > 0.0001 {
		return new.diff < old.diff
	}

	return new.entropy > old.entropy
}

// Format helpers
func formatDuration(_ seconds: Double) -> String {
	if seconds.isInfinite {
		return "∞"
	}

	if seconds >= 86400 {
		return String(format: "%.3f 天", seconds / 86400.0)
	}
	if seconds >= 3600 {
		return String(format: "%.3f 小时", seconds / 3600.0)
	}
	if seconds >= 60 {
		return String(format: "%.3f 分钟", seconds / 60.0)
	}
	return String(format: "%.3f 秒", seconds)
}

func getOverlapStats(
	steps: [Step],
	battery: Config.Battery,
	actualBatteryCount: Int,
	requiredPower: Double,
	analyzedBatteryCount: Int,
	preSplit: Double,
	minOverflow: Double
) -> OverlapStats {
	// Simulation logic:
	// - Core used when battery depleted.
	// - Core charges on excess (waste after full).
	// - Transport delay: 1 tile / 2s.

	let rootPeriod: Int = Int(2 * preSplit)
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
			// Sync stream delays
			activeStreams.append(
				Stream(period: branchP, offset: branchO)
			)
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

	// Simulate 2 cycles; measure stats on the 2nd to ensure stable state.
	let singleCycleDuration = Double(cycle)

	// Generate events with splitter delays
	var arrivalTimes: [Double] = []
	for s in activeStreams {
		let startPhase = s.offset % cycle
		var t = startPhase
		// Generate one cycle of events
		while t < cycle {
			let rawTime = Double(t) * 2.0
			let time = rawTime.truncatingRemainder(dividingBy: singleCycleDuration)
			arrivalTimes.append(time)
			t += s.period
		}
	}
	arrivalTimes.sort()

	let baseArrivals = arrivalTimes

	// Net benefit constants
	let oneBatteryTotalEnergy = battery.totalEnergy
	let excessPowerWithoutSplit = battery.power * Double(actualBatteryCount) - requiredPower

	let saveRateInBatteriesPerSec =
		(excessPowerWithoutSplit > 0.001)
		? (excessPowerWithoutSplit / oneBatteryTotalEnergy) : 0.0

	if baseArrivals.isEmpty {
		return OverlapStats(
			cycleTime: singleCycleDuration,
			overflow: 0.0,
			minLevel: coreMaxCapacity,
			endLevel: coreMaxCapacity,
			profile: OverlapProfile(
				overflowPerSecond: minOverflow,
				minBatteryLevel: coreMaxCapacity,
				endBatteryLevel: coreMaxCapacity,
				hitFullCharge: true,
				netBenefitPerSecond: saveRateInBatteriesPerSec
					- (minOverflow / oneBatteryTotalEnergy),
				outageDurationPer1000Sec: 0.0
			)
		)
	}

	// Adjust simulation duration for constant complexity
	// Base 24h
	let baseTargetDuration = 86400.0
	let adjustedTargetDuration = baseTargetDuration / Double(max(1, analyzedBatteryCount))

	let computedCycles = Int(ceil(adjustedTargetDuration / singleCycleDuration))

	// Enforce min simulation coverage (12h for short cycles, 1 cycle for long)
	var simCycleCount = max(1, computedCycles)
	if singleCycleDuration < 3600.0 {
		let shortCycleTarget = Int(ceil(43200.0 / singleCycleDuration))
		simCycleCount = max(simCycleCount, shortCycleTarget)
	}

	// Measure after warmup cycle unless cycle is very long (covering 20h+ without warmup is fine).
	let measureStart = (simCycleCount > 1) ? singleCycleDuration : 0.0
	let measureEnd = Double(simCycleCount) * singleCycleDuration
	let recoveryCheckStart = measureEnd - singleCycleDuration

	var allArrivals: [Double] = []
	for i in 0..<simCycleCount {
		for t in baseArrivals {
			allArrivals.append(t + Double(i) * singleCycleDuration)
		}
	}

	// Round-robin generator distribution
	var genArrivals: [[Double]] = Array(repeating: [], count: analyzedBatteryCount)

	for (i, t) in allArrivals.enumerated() {
		let index = i % analyzedBatteryCount
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
		// Process end events before start events at same timestamp
		if abs(a.time - b.time) < 0.0001 {
			return a.type < b.type
		}
		return a.time < b.time
	}

	// Measurement Window: [cycleTime, simCycleCount*cycleTime]
	let measurementDuration = measureEnd - measureStart

	var active = 0
	var lastT = 0.0
	// Start full
	var currentLevel = coreMaxCapacity

	// Measurement window stats
	var totalOverflow = 0.0
	var totalOutageTime = 0.0
	var minLevel = coreMaxCapacity
	var hitFullCharge = false

	var minLevelInitialized = (measureStart <= 0.0001)

	for e in pevents {
		let t = e.time
		let dt = t - lastT

		if dt > 0.00001 {
			// Integrate state over dt
			let inputPower = Double(active) * battery.power
			let netPower = inputPower - requiredPower

			// Check window intersection
			let segStart = lastT
			let segEnd = t
			let overlapStart = max(segStart, measureStart)
			let overlapEnd = min(segEnd, measureEnd)
			let overlapDt = overlapEnd - overlapStart

			let levelBefore = currentLevel
			let possibleLevel = currentLevel + netPower * dt

			var nextLevel = possibleLevel

			if possibleLevel > coreMaxCapacity {
				nextLevel = coreMaxCapacity
			} else if possibleLevel <= 0 {
				nextLevel = 0
			}

			// Calculate stats strictly within overlap window, accounting for clamping
			if overlapDt > 0.000001 {
				// Project level at overlapStart
				let dtPre = max(0, overlapStart - lastT)
				var levelAtWindowEntry = levelBefore
				if dtPre > 0 {
					let p = levelBefore + netPower * dtPre
					levelAtWindowEntry = min(coreMaxCapacity, max(0, p))
				}

				// Simulate from levelAtWindowEntry over overlapDt
				let pEnd = levelAtWindowEntry + netPower * overlapDt

				if pEnd > coreMaxCapacity {
					totalOverflow += (pEnd - coreMaxCapacity)
				}

				if pEnd <= 0 {
					let timeToZeroLocal =
						(netPower < -0.00001) ? (levelAtWindowEntry / abs(netPower)) : 0.0
					let outageLocal = max(0, overlapDt - timeToZeroLocal)
					totalOutageTime += outageLocal
				}

				// Track minLevel within window
				if !minLevelInitialized {
					minLevel = levelAtWindowEntry
					minLevelInitialized = true
				} else {
					minLevel = min(minLevel, levelAtWindowEntry)
				}
				let levelAtWindowExit = min(coreMaxCapacity, max(0, pEnd))
				minLevel = min(minLevel, levelAtWindowExit)

				// Track full charge recovery (sustainability check)
				if overlapEnd >= recoveryCheckStart {
					if levelAtWindowExit >= (coreMaxCapacity - 0.001) {
						hitFullCharge = true
					}
				}
			}

			currentLevel = nextLevel
		}

		active += e.type
		lastT = t

		if lastT >= measureEnd {
			break
		}
	}

	// Handle tail
	if lastT < measureEnd {
		let dt = measureEnd - lastT
		if dt > 0.00001 {
			if !minLevelInitialized {
				minLevel = currentLevel
				minLevelInitialized = true
			}

			let inputPower = Double(active) * battery.power
			let netPower = inputPower - requiredPower

			let possibleLevel = currentLevel + netPower * dt

			if possibleLevel > coreMaxCapacity {
				totalOverflow += (possibleLevel - coreMaxCapacity)
				currentLevel = coreMaxCapacity
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

	// Fallback for empty windows
	if !minLevelInitialized {
		minLevel = currentLevel
	}

	let measuredOverflowPerSec = totalOverflow / measurementDuration
	let overflowPerSec = max(measuredOverflowPerSec, minOverflow)

	let wasteRateInBatteriesPerSec =
		(overflowPerSec > 0.000001) ? (overflowPerSec / oneBatteryTotalEnergy) : 0.0
	let netBenefitPerSec = saveRateInBatteriesPerSec - wasteRateInBatteriesPerSec

	let outageDurationPer1000Sec =
		(measurementDuration > 0.001) ? ((totalOutageTime / measurementDuration) * 1000.0) : 0.0

	let profile = OverlapProfile(
		overflowPerSecond: overflowPerSec,
		minBatteryLevel: minLevel,
		endBatteryLevel: currentLevel,
		hitFullCharge: hitFullCharge,
		netBenefitPerSecond: netBenefitPerSec,
		outageDurationPer1000Sec: outageDurationPer1000Sec
	)

	return OverlapStats(
		cycleTime: measurementDuration,
		overflow: totalOverflow,
		minLevel: minLevel,
		endLevel: currentLevel,
		profile: profile
	)
}

// --- Analysis Helper ---
func analyzeSolutionOverlap(
	_ solution: Solution,
	battery: Config.Battery,
	batteryStatic: Config.Battery
) {
	let preSplit = solution.preSplitBits.reduce(1.0) {
		$0 * pow(Double($1.key), Double($1.value))
	}
	let stats = getOverlapStats(
		steps: solution.steps,
		battery: battery,
		actualBatteryCount: solution.actualBatteryCount,
		requiredPower: solution.requiredPower,
		analyzedBatteryCount: solution.analyzedBatteryCount,
		preSplit: preSplit,
		minOverflow: solution.diff
	)

	let oneBatteryTotalEnergyGlob = battery.totalEnergy
	let inputDoubleBatteryPowerGlob = battery.power * Double(solution.actualBatteryCount)
	let baselineBatteriesPerDayGlob =
		(inputDoubleBatteryPowerGlob * 86400.0) / oneBatteryTotalEnergyGlob
	let requiredBatteriesPerDayGlob =
		(solution.requiredPower * 86400.0) / oneBatteryTotalEnergyGlob
	let possibleSavePerDayGlob = baselineBatteriesPerDayGlob - requiredBatteriesPerDayGlob

	print(
		"\n\t📦 电池数量:　\(batteryStatic.name): \(solution.staticBatteryCount)个, \(battery.name): \(solution.actualBatteryCount)个"
	)
	print(
		"\t📉 基准消耗:　\(String(format: "%.3f", baselineBatteriesPerDayGlob)) 颗/天 (\(solution.actualBatteryCount)发电机常开)"
	)
	print("\t🎯 理论最少:　\(String(format: "%.3f", requiredBatteriesPerDayGlob)) 颗/天 (100%利用率)")
	print("\t------------------------------------------------")

	let netBenefit = stats.profile.netBenefitPerSecond
	let savedPerDay = (netBenefit > 0) ? (netBenefit * 86400.0) : 0.0

	// print("\n==================================================")

	print("\t🛠 操作步骤:　\(solution.allActions)")

	print("\t🔄 周期:　　　\(String(format: "%.3f", stats.cycleTime))秒")
	print("\t📉 最低电量:　\(String(format: "%.4f", stats.minLevel))")
	print("\t📊 结束电量:　\(String(format: "%.4f", stats.endLevel))")
	print("\t✅ 满电复位:　\(stats.profile.hitFullCharge ? "是" : "否")")
	if stats.minLevel < 0.001 {
		let outageStr = String(format: "%.3f", stats.profile.outageDurationPer1000Sec)
		print("\t⚠️ 警告:　　该方案可能会导致短暂停电 (最低电量归零), 平均停电: \(outageStr)秒/1000秒 ⚠️")
	}
	print("\t🔌 最终功率:　\(String(format: "%.4f", solution.finalC))")
	print("\t⚖️ 差值:　　　\(String(format: "%.4f", solution.diff))")

	let oneBatteryTotalEnergy = battery.totalEnergy
	let inputBatteryPower = battery.power * Double(solution.actualBatteryCount)

	// 1. Calc waste rate (accounting for inevitable overflow if power > required)
	let effectiveOverflow = max(stats.profile.overflowPerSecond, solution.diff)
	let secondsToWasteOneBattery: Double =
		(effectiveOverflow > 0.001)
		? (oneBatteryTotalEnergy / effectiveOverflow) : Double.infinity

	// 2. Calc save time
	let excessPowerWithoutSplit = inputBatteryPower - solution.requiredPower
	let secondsToSaveOneBattery =
		(excessPowerWithoutSplit > 0.001)
		? (oneBatteryTotalEnergy / excessPowerWithoutSplit) : Double.infinity

	print(
		"\t⏳ 理论每:　　\(formatDuration(secondsToSaveOneBattery)) 省一颗【\(battery.name)】 (基准 \(solution.actualBatteryCount)发电机满载)"
	)
	print("\t🗑 实际每:　　\(formatDuration(secondsToWasteOneBattery)) 溢出一颗【\(battery.name)】")
	print(
		"\t💎 净收益:　　\(String(format: "%.6f", netBenefit)) 颗/秒  ≈  \(String(format: "%.3f", savedPerDay))颗/天(理论可省: \(String(format: "%.3f", possibleSavePerDayGlob)) 颗/天, 差值: \(String(format: "%.3f", possibleSavePerDayGlob - savedPerDay)))"
	)
}

struct OverlapStats {
	let cycleTime: Double
	let overflow: Double
	let minLevel: Double
	let endLevel: Double
	let profile: OverlapProfile
}

for config in configs {
	print("\n--------------------------------------------------")
	print("🔍 正在搜索:　\(config.name)")

	let batteryStatic = config.staticBattery
	let battery = config.analyzedBattery

	// Total required power (minus 200W core base)
	let totalPower = config.baseRequiredPower - 200

	// Number of batteries to split
	var analyzedBatteryCount: Int = Int(ceil(totalPower / battery.power))

	var solutions: [[Int: Int]: [Solution?]] = [:]

	while analyzedBatteryCount > 0 {
		defer {
			analyzedBatteryCount -= 1
		}
		let maxAnalyzedBatteryDesignPower = battery.power * Double(analyzedBatteryCount)
		let maxAnalyzedBatteryPower = min(totalPower, maxAnalyzedBatteryDesignPower)
		let staticBatteryCount = Int(
			ceil((totalPower - maxAnalyzedBatteryPower) / batteryStatic.power))
		let requiredPower: Double = totalPower - Double(staticBatteryCount) * batteryStatic.power

		let actualBatteryCount = Int(ceil(requiredPower / battery.power))

		// --- Derived vars ---

		var allPreSplitBits: Set<[Int: Int]> = []
		func findBits(previousPower: Double, splitFactor: Double, bits: [Int: Int]) {
			let preSplit = bits.reduce(1.0) {
				$0 * pow(Double($1.key), Double($1.value))
			}
			guard preSplit <= Double(maxDepthLimit) else {
				return
			}
			let currentPower = previousPower * splitFactor
			let maxRemainingBitCount = Int(ceil(log2(currentPower)))
			let bitLimit = Int(preSplit) - extraBeltInSteps
			guard maxRemainingBitCount >= bitLimit else {
				return
			}
			allPreSplitBits.insert(bits)
			findBits(
				previousPower: currentPower,
				splitFactor: 1 / 2,
				bits: {
					var bits = bits
					bits[2, default: 0] += 1
					return bits
				}()
			)
			findBits(
				previousPower: currentPower,
				splitFactor: 1 / 3,
				bits: {
					var bits = bits
					bits[3, default: 0] += 1
					return bits
				}()
			)
		}
		findBits(
			previousPower: battery.totalEnergy / 2,
			splitFactor: 1,
			bits: [:]
		)

		// Iterate all pre-split combinations
		for preSplitBits in allPreSplitBits {
			if solutions[preSplitBits] == nil {
				solutions[preSplitBits] = Array(repeating: nil, count: maxDepthLimit)
			}
			let preSplit = preSplitBits.reduce(1.0) {
				$0 * pow(Double($1.key), Double($1.value))
			}
			let depthLimit = min(maxDepthLimit, Int(preSplit) - extraBeltInSteps)

			let totalBatteryPower: Double = battery.totalEnergy / 2 / preSplit

			guard battery.power * Double(actualBatteryCount) > requiredPower else {
				continue
			}

			// Recursive search with pruning
			func binarySplit(
				sourceVal: Double,
				testBattery: Double,
				entropy: Double,
				n: Int,
				steps: [Step],
				values: [Double]
			) {
				guard
					sourceVal >= 1.0,
					steps.count < depthLimit,
					testBattery + sourceVal > requiredPower
				else {
					return
				}

				let diff = testBattery - requiredPower

				// Pruning 1: Exceeded requirement (cannot decrease)
				guard diff <= 50 else {
					return
				}

				// Record solution. 'diff' sets minOverflow to penalize high-waste solutions.
				if diff >= allowedMinDiff, steps.last?.action == .add {
					let profile = getOverlapStats(
						steps: steps,
						battery: battery,
						actualBatteryCount: actualBatteryCount,
						requiredPower: requiredPower,
						analyzedBatteryCount: analyzedBatteryCount,
						preSplit: preSplit,
						minOverflow: diff
					).profile

					let sol = Solution(
						finalC: testBattery,
						entropy: entropy,
						preSplitBits: preSplitBits,
						steps: steps,
						splitValues: values,
						diff: diff,
						overlap: profile,
						actualBatteryCount: actualBatteryCount,
						requiredPower: requiredPower,
						analyzedBatteryCount: analyzedBatteryCount,
						staticBatteryCount: staticBatteryCount,
						depthLimit: depthLimit
					)
					let index = steps.count
					if let existing = solutions[preSplitBits]![index] {
						if isBetterSolution(sol, than: existing) {
							solutions[preSplitBits]![index] = sol
						}
					} else {
						solutions[preSplitBits]![index] = sol
					}
				}

				// Pruning 2: Max possible power check (Upper bound assumption)
				let remaining = Double(depthLimit - steps.count)
				let maxPotential = sourceVal * (1.0 - pow(0.5, remaining))

				if testBattery + maxPotential < requiredPower {
					return
				}

				// Binary Split

				let half = sourceVal / 2.0

				// Branch 1: Add
				binarySplit(
					sourceVal: half,
					testBattery: testBattery + half,
					entropy: entropy,
					n: n + 1,
					steps: steps + [Step(type: .two, action: .add)],
					values: values + [half]
				)

				// Branch 2: Discard (entropy +)
				let entropyIncrement = 1.0 / pow(2.0, Double(n))
				binarySplit(
					sourceVal: half,
					testBattery: testBattery,
					entropy: entropy + entropyIncrement,
					n: n + 1,
					steps: steps + [Step(type: .two, action: .discard)],
					values: values + [half]
				)

				guard enableThree else {
					return
				}

				// Ternary Split
				let third = sourceVal / 3.0

				// Branch 3: Add
				binarySplit(
					sourceVal: third,
					testBattery: testBattery + third,
					entropy: entropy,
					n: n,
					steps: steps + [Step(type: .three, action: .add)],
					values: values + [third]
				)

				// Branch 4: Discard
				binarySplit(
					sourceVal: third,
					testBattery: testBattery,
					entropy: entropy,
					n: n,
					steps: steps + [Step(type: .three, action: .discard)],
					values: values + [third]
				)
			}

			// Start search

			binarySplit(
				sourceVal: totalBatteryPower,
				testBattery: 0.0,
				entropy: 0.0,
				n: 0,
				steps: [],
				values: []
			)
		}
	}

	var uniqueSolutions: [String: Solution] = [:]
	for sol in solutions.values.flatMap(\.self).compactMap(\.self) {
		let key = sol.allActions
		if let existing = uniqueSolutions[key] {
			if isBetterSolution(sol, than: existing) {
				uniqueSolutions[key] = sol
			}
		} else {
			uniqueSolutions[key] = sol
		}
	}

	let tops = uniqueSolutions.values.sorted(by: { (a, b) -> Bool in
		return isBetterSolution(a, than: b)
	}).prefix(showTopSolutions)

	guard !tops.isEmpty else {
		print("\n==================================================")
		print("❌ 未找到任何符合条件的方案")
		print("==================================================")
		continue
	}

	for (index, sol) in tops.enumerated() {
		if index > 0 {
			print("\n\t==================================================")
			print("\n")
		}
		analyzeSolutionOverlap(
			sol,
			battery: battery,
			batteryStatic: batteryStatic
		)
	}
}
