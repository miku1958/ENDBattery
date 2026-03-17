// swift-min-version: 5.2

import Foundation

/* ————————————————————————————— 需要认真填的数据 ————————————————————————————— */

let configs: [Config] = [
	Config(
		name: "4号谷地",
		/// 固定消耗的电池: .originium, .green, .blue, .purple, .lowEarth
		staticBattery: .purple,

		/// 需要分流的电池: .originium, .green, .blue, .purple, .lowEarth
		analyzedBattery: .purple,

		/// 你屏幕上显示的总功率需求
		baseRequiredPower: 5060
	),

	Config(
		name: "武陵",
		/// 固定消耗的电池: .originium, .green, .blue, .purple, .lowEarth
		staticBattery: .originium,

		/// 需要分流的电池: .originium, .green, .blue, .purple, .lowEarth
		analyzedBattery: .midEarth,

		/// 你屏幕上显示的总功率需求
		baseRequiredPower: 2500
	),
]

/* ————————————————————————————— 选填的数据 ————————————————————————————— */

/// 最少分流电池数, 调高会过滤掉一些方案, 一般不需要改
let minAnalyzedBatteryCount: Int = 1

/// 递归时额外的传送带/分流器数量（格）, 一般不用改
/// 用于限制递归上限
/// 最小值为 1
let extraBeltInSteps: Int = 1

/// 最大递归深度, 会影响结果的长度和运行的速度, 这个值越大越慢, 但可能找到更优解
let maxDepthLimit: Int = 9

/// 最终输出的方案数量, 1会输出最优方案, 3会输出前三方案, 以此类推
var showTopSolutions: Int = 1

/// 允许最小差值, 这个值越小越接近理论最优, 但是会下线后因为鹰角的服务器优化而导致计算不正确, 如果出现这种情况可以适当调大这个值到10以上
var allowedMinDiff: Double = 10

/// 是否允许使用三分流（增加搜索空间和时间, 但可能找到更优解）
let enableThree: Bool = true

/// 安全阈值: 如果电量曾经低于这个值, 方案会被严重降级. 这个值越高越安全, 但可能错过一些边缘方案. 建议设置在15%左右 (15000), 也可以根据需求调整.
let safetyThreshold = 0.15

/// 方案停止工作后（没有分流电池流入）到停电的最大允许时间（秒）
/// - nil: 自动使用 getOverlap Stats 计算出的“全分流发电机同时停机最长连续时长”作为校验间隔
/// - 例如 1800: 要求方案在停流后至少可坚持 30 分钟不断电
let maxStopToOutageSeconds: Double? = nil

/// 允许的最大缺电时间（秒）
/// 缺电定义：当前发电机总功率 < 需求功率，导致开始消耗核心电量。
/// 缺电结束：发电机总功率 >= 需求功率，且此次充电过程能将核心电量充满。
let maxShortageDurationLimitInSecond: Double = 40

/// 保留所有结果并在最后对所有结果进行测试
/// 关闭时会提前过滤掉一些方案以减少最后测试的压力, 但也足够准确了.
/// 开启后会保留所有方案并在最后测试后再进行排序, 超慢(慢500x), 并且开和关的结果大概率是一样的
let keepAllSolutions: Bool = false

/* ————————————————————————————— 下面不用看 ————————————————————————————— */

let startTime = Date()

defer {
	let elapsed = Date().timeIntervalSince(startTime)
	print(String(format: "\n🕐 总耗时: %.2f 秒", elapsed))
}

/// Core capacity.
let coreMaxCapacity: Double = 100000

/// Belt speed: seconds per tile.
let beltSecondsPerTile: Int = 2

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
		case midEarth

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
			case .midEarth:
				return "中容息壤电池"
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
			case .midEarth:
				return 3200
			}
		}

		var life: Double {
			switch self {
			case .originium:
				return 8
			case .green, .blue, .purple, .lowEarth, .midEarth:
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

struct SplitterConfig {
	let branchSlot: Int
	let continuationSlot: Int
}

func defaultSplitterConfigs(for steps: [Step]) -> [SplitterConfig] {
	return steps.map { _ in SplitterConfig(branchSlot: 0, continuationSlot: 1) }
}

func enumerateSplitterConfigs(for steps: [Step]) -> [[SplitterConfig]] {
	var result: [[SplitterConfig]] = [[]]
	for step in steps {
		let slotCount = (step.type == .three) ? 3 : 2
		var newResult: [[SplitterConfig]] = []
		for existing in result {
			if step.action == .add {
				for b in 0..<slotCount {
					for c in 0..<slotCount where c != b {
						var config = existing
						config.append(SplitterConfig(branchSlot: b, continuationSlot: c))
						newResult.append(config)
					}
				}
			} else {
				for c in 0..<slotCount {
					let b = (c == 0) ? 1 : 0
					var config = existing
					config.append(SplitterConfig(branchSlot: b, continuationSlot: c))
					newResult.append(config)
				}
			}
		}
		result = newResult
	}
	return result
}

func computeActiveStreams(
	steps: [Step],
	rootPeriod: Int,
	splitterConfigs: [SplitterConfig]
) -> [(period: Int, offset: Int)] {
	var currentP = rootPeriod
	var currentO = 0
	var streams: [(period: Int, offset: Int)] = []

	for (i, step) in steps.enumerated() {
		let multiplier = (step.type == .three) ? 3 : 2
		let config = splitterConfigs[i]

		if step.action == .add {
			let branchO = currentO + config.branchSlot * currentP
			streams.append((period: currentP * multiplier, offset: branchO))
		}

		currentO = currentO + config.continuationSlot * currentP
		currentP = currentP * multiplier
	}

	return streams
}

func enumerateUniqueStreamSets(
	steps: [Step],
	rootPeriod: Int
) -> [[(period: Int, offset: Int)]] {
	let allConfigs = enumerateSplitterConfigs(for: steps)

	var seenKeys: Set<[Int]> = []
	var result: [[(period: Int, offset: Int)]] = []

	for configs in allConfigs {
		let streams = computeActiveStreams(
			steps: steps, rootPeriod: rootPeriod, splitterConfigs: configs)

		guard !streams.isEmpty else {
			let emptyKey: [Int] = []
			if seenKeys.insert(emptyKey).inserted {
				result.append([])
			}
			continue
		}

		let shift = streams[0].offset
		var normalizedKey: [Int] = []
		normalizedKey.reserveCapacity(streams.count * 2)
		for s in streams {
			let normOffset = ((s.offset - shift) % s.period + s.period) % s.period
			normalizedKey.append(s.period)
			normalizedKey.append(normOffset)
		}

		if seenKeys.insert(normalizedKey).inserted {
			result.append(streams)
		}
	}

	return result
}

struct OverlapProfile: Comparable {
	let overflowPerSecond: Double
	let minBatteryLevel: Double
	let endBatteryLevel: Double
	let hitFullCharge: Bool

	/// Net batteries saved per second
	let netBenefitPerSecond: Double

	/// Max consecutive shortage duration
	let maxShortageDuration: Double

	static func < (lhs: OverlapProfile, rhs: OverlapProfile) -> Bool {
		// First check if either exceeds the limit
		let lhsExceeds = lhs.maxShortageDuration > maxShortageDurationLimitInSecond
		let rhsExceeds = rhs.maxShortageDuration > maxShortageDurationLimitInSecond

		if lhsExceeds && !rhsExceeds {
			return false  // lhs is invalid, so rhs is (likely) better
		}
		if !lhsExceeds && rhsExceeds {
			return true  // lhs is valid, so better
		}
		if lhsExceeds && rhsExceeds {
			// Both invalid, prefer smaller violation
			return lhs.maxShortageDuration < rhs.maxShortageDuration
		}

		// Both are valid (<= limit). Proceed with standard optimization goals.

		// Safety check: Avoid critically low battery levels.
		let lhsLow = lhs.minBatteryLevel < coreMaxCapacity * safetyThreshold
		let rhsLow = rhs.minBatteryLevel < coreMaxCapacity * safetyThreshold
		if lhsLow || rhsLow {
			if abs(lhs.minBatteryLevel - rhs.minBatteryLevel) > 1e-4 {
				return lhs.minBatteryLevel > rhs.minBatteryLevel
			}
		}

		// Priority: Max Net Benefit (Saving batteries)
		if abs(lhs.netBenefitPerSecond - rhs.netBenefitPerSecond) > 1e-4 {
			return lhs.netBenefitPerSecond > rhs.netBenefitPerSecond
		}

		// Priority: Lower Shortage Duration (as tie-breaker for same benefit)
		if abs(lhs.maxShortageDuration - rhs.maxShortageDuration) > 1e-4 {
			return lhs.maxShortageDuration < rhs.maxShortageDuration
		}

		// Priority: Smaller Overflow
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
	var overlap: OverlapProfile
	let actualBatteryCount: Int
	let requiredPower: Double
	let analyzedBatteryCount: Int
	let staticBatteryCount: Int
	let depthLimit: Int
	let stopToOutageSeconds: Double
	let requiredStopIntervalSeconds: Double

	init(
		finalC: Double, entropy: Double, preSplitBits: [Int: Int], steps: [Step],
		splitValues: [Double], diff: Double, overlap: OverlapProfile, actualBatteryCount: Int,
		requiredPower: Double, analyzedBatteryCount: Int, staticBatteryCount: Int, depthLimit: Int,
		stopToOutageSeconds: Double, requiredStopIntervalSeconds: Double
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
		self.stopToOutageSeconds = stopToOutageSeconds
		self.requiredStopIntervalSeconds = requiredStopIntervalSeconds
	}

	lazy var totalStepCount: Int = {
		var count = 0
		for (_, c) in preSplitBits { count += c }
		count += steps.count
		return count
	}()

	lazy var allActions: String = {
		var allSteps: [(type: Int, action: Action)] = []

		for (bit, count) in preSplitBits {
			for _ in 0..<count {
				allSteps.append((type: bit, action: .discard))
			}
		}

		for step in steps {
			let bit = (step.type == .two) ? 2 : 3
			allSteps.append((type: bit, action: step.action))
		}

		// Group consecutive same-action steps, sort discards (3 before 2) within each group
		var actionGroups: [[(type: Int, action: Action)]] = []
		if !allSteps.isEmpty {
			var currentGroup: [(type: Int, action: Action)] = [allSteps[0]]
			for i in 1..<allSteps.count {
				if allSteps[i].action == allSteps[i - 1].action {
					currentGroup.append(allSteps[i])
				} else {
					actionGroups.append(currentGroup)
					currentGroup = [allSteps[i]]
				}
			}
			actionGroups.append(currentGroup)
		}

		var orderedSteps: [(type: Int, action: Action)] = []
		for group in actionGroups {
			if group[0].action == .discard {
				orderedSteps.append(contentsOf: group.sorted { $0.type > $1.type })
			} else {
				orderedSteps.append(contentsOf: group)
			}
		}

		guard !orderedSteps.isEmpty else { return "" }

		// Run-length encode consecutive identical (type + action) steps
		var result: [String] = []
		var curType = orderedSteps[0].type
		var curAction = orderedSteps[0].action
		var count = 1

		for i in 1..<orderedSteps.count {
			let s = orderedSteps[i]
			if s.type == curType && s.action == curAction {
				count += 1
			} else {
				let actStr = (curAction == .add) ? "🟢" : "🔴"
				result.append(count > 1 ? "\(curType)\(actStr)×\(count)" : "\(curType)\(actStr)")
				curType = s.type
				curAction = s.action
				count = 1
			}
		}
		let actStr = (curAction == .add) ? "🟢" : "🔴"
		result.append(count > 1 ? "\(curType)\(actStr)×\(count)" : "\(curType)\(actStr)")

		return result.joined(separator: "     ")
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
		return String(format: "%.3f 天", seconds / 86400)
	}
	if seconds >= 3600 {
		return String(format: "%.3f 小时", seconds / 3600)
	}
	if seconds >= 60 {
		return String(format: "%.3f 分钟", seconds / 60)
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
	minOverflow: Double,
	splitterConfigs: [SplitterConfig]? = nil,
	precomputedStreams: [(period: Int, offset: Int)]? = nil,
	computeLongestCycle: Bool = false
) -> OverlapStats {

	let rootPeriod: Int = Int((Double(beltSecondsPerTile) * preSplit).rounded())
	struct Stream {
		let period: Int
		let offset: Int
	}
	var activeStreams: [Stream]

	if let pre = precomputedStreams {
		activeStreams = pre.map { Stream(period: $0.period, offset: $0.offset) }
	} else {
		activeStreams = []
		var currentP = rootPeriod
		var currentO = 0

		let configs = splitterConfigs ?? defaultSplitterConfigs(for: steps)

		for (i, step) in steps.enumerated() {
			let isTernary = (step.type == .three)
			let multiplier = isTernary ? 3 : 2
			let config = configs[i]

			let branchP = currentP * multiplier
			let branchO = currentO + config.branchSlot * currentP

			let nextP = currentP * multiplier
			let nextO = currentO + config.continuationSlot * currentP

			if step.action == .add {
				activeStreams.append(
					Stream(period: branchP, offset: branchO)
				)
			}

			currentP = nextP
			currentO = nextO
		}
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

	// Generate events with splitter delays via sorted merge
	var perStreamArrivals: [[Double]] = []
	for s in activeStreams {
		var arrivals: [Double] = []
		let startPhase = s.offset % cycle
		var t = startPhase
		while t < cycle {
			arrivals.append(Double(t))
			t += s.period
		}
		perStreamArrivals.append(arrivals)
	}

	// Merge sorted arrays without closure-based sort
	var baseArrivals: [Double] = []
	for arr in perStreamArrivals {
		if baseArrivals.isEmpty {
			baseArrivals = arr
		} else {
			var merged: [Double] = []
			merged.reserveCapacity(baseArrivals.count + arr.count)
			var i = 0
			var j = 0
			while i < baseArrivals.count && j < arr.count {
				if baseArrivals[i] <= arr[j] {
					merged.append(baseArrivals[i])
					i += 1
				} else {
					merged.append(arr[j])
					j += 1
				}
			}
			while i < baseArrivals.count {
				merged.append(baseArrivals[i])
				i += 1
			}
			while j < arr.count {
				merged.append(arr[j])
				j += 1
			}
			baseArrivals = merged
		}
	}

	// Net benefit constants
	let oneBatteryTotalEnergy = battery.totalEnergy
	let excessPowerWithoutSplit = battery.power * Double(actualBatteryCount) - requiredPower

	let saveRateInBatteriesPerSec: Double =
		(excessPowerWithoutSplit > 0)
		? (excessPowerWithoutSplit / oneBatteryTotalEnergy) : 0

	if baseArrivals.isEmpty {
		return OverlapStats(
			cycleTime: singleCycleDuration,
			overflow: 0,
			minLevel: coreMaxCapacity,
			endLevel: coreMaxCapacity,
			maxAllStoppedDuration: singleCycleDuration,
			profile: OverlapProfile(
				overflowPerSecond: minOverflow,
				minBatteryLevel: coreMaxCapacity,
				endBatteryLevel: coreMaxCapacity,
				hitFullCharge: true,
				netBenefitPerSecond: saveRateInBatteriesPerSec
					- (minOverflow / oneBatteryTotalEnergy),
				maxShortageDuration: 0
			),
			longestFullChargeCycle: 0
		)
	}

	// The system is periodic. The true period accounts for round-robin
	// generator distribution: rrPeriod = lcm(arrivalsPerCycle, analyzedBatteryCount) / arrivalsPerCycle.
	// Simulate 1 warmup cycle + rrPeriod measurement cycles.
	let arrivalsPerCycle = baseArrivals.count
	let rrPeriod =
		(arrivalsPerCycle > 0)
		? analyzedBatteryCount / gcd(arrivalsPerCycle, analyzedBatteryCount) : 1
	let simCycleCount = 1 + rrPeriod
	let measureStart: Double = (simCycleCount > 1) ? singleCycleDuration : 0
	let measureEnd: Double = Double(simCycleCount) * singleCycleDuration
	let recoveryCheckStart: Double = measureEnd - singleCycleDuration

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
		var nextAvailable: Double = 0
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

	// Build sorted events via merge instead of closure-based sort
	var pevents: [EventPoint]
	if analyzedBatteryCount == 1 {
		pevents = []
		pevents.reserveCapacity(combinedIntervals.count * 2)
		for inv in combinedIntervals {
			if abs(inv.start - inv.end) < 0.0001 {
				pevents.append(EventPoint(time: inv.start, type: 1))
				pevents.append(EventPoint(time: inv.end, type: -1))
			} else {
				pevents.append(EventPoint(time: inv.start, type: 1))
				pevents.append(EventPoint(time: inv.end, type: -1))
			}
		}
	} else {
		var perGenEvents: [[EventPoint]] = []
		var offset = 0
		for arrivals in genArrivals {
			let intervals = Array(combinedIntervals[offset..<(offset + arrivals.count)])
			offset += arrivals.count
			var evts: [EventPoint] = []
			evts.reserveCapacity(intervals.count * 2)
			for inv in intervals {
				evts.append(EventPoint(time: inv.start, type: 1))
				evts.append(EventPoint(time: inv.end, type: -1))
			}
			perGenEvents.append(evts)
		}

		pevents = []
		for evts in perGenEvents {
			if pevents.isEmpty {
				pevents = evts
			} else {
				var merged: [EventPoint] = []
				merged.reserveCapacity(pevents.count + evts.count)
				var i = 0
				var j = 0
				while i < pevents.count && j < evts.count {
					let useI: Bool
					if abs(pevents[i].time - evts[j].time) < 0.0001 {
						useI = pevents[i].type <= evts[j].type
					} else {
						useI = pevents[i].time < evts[j].time
					}
					if useI {
						merged.append(pevents[i])
						i += 1
					} else {
						merged.append(evts[j])
						j += 1
					}
				}
				while i < pevents.count {
					merged.append(pevents[i])
					i += 1
				}
				while j < evts.count {
					merged.append(evts[j])
					j += 1
				}
				pevents = merged
			}
		}
	}

	// Measurement Window: [cycleTime, simCycleCount*cycleTime]
	let measurementDuration = measureEnd - measureStart

	var active: Int = 0
	var lastT: Double = 0
	// Start full
	var currentLevel = coreMaxCapacity

	// Measurement window stats
	var totalOverflow: Double = 0
	var minLevel = coreMaxCapacity
	var hitFullCharge = false
	var maxAllStoppedDuration: Double = 0
	var currentAllStoppedStreak: Double = 0
	var maxShortageDuration: Double = 0

	var fullChargeTimestamps: [Double] = []

	// Shortage tracking
	// shortageStartTime: The time when the current shortage sequence began.
	// If nil, we are not in a shortage (or we are fully charged/safe).
	var shortageStartTime: Double? = nil

	// potentialEndCandidate: When charging starts, we MIGHT be ending the shortage.
	// We store the time when charging started. If it hits full, the shortage ended at this time.
	// If it drops again without hitting full, this candidate is invalid and shortage continues.
	var potentialEndCandidate: Double? = nil

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

			if computeLongestCycle
				&& levelBefore < coreMaxCapacity - 0.001
				&& possibleLevel >= coreMaxCapacity - 0.001
			{
				if netPower > 0.001 {
					let timeToFull = (coreMaxCapacity - levelBefore) / netPower
					let tFull = lastT + timeToFull
					if tFull >= measureStart && tFull <= measureEnd {
						fullChargeTimestamps.append(tFull)
					}
				}
			}

			// Logic for Shortage Duration:
			// - Start: netPower < 0 (draining) while at full capacity or not currently tracking a shortage.
			// - End: netPower >= 0 (charging) leads to a full charge state.
			//   The shortage ends at the beginning of the charging phase that successfully reaches full capacity.

			let isDraining = (netPower < -0.00001)

			if isDraining {
				// Start tracking shortage if not already doing so
				if shortageStartTime == nil {
					if levelBefore < coreMaxCapacity {
						// Case: Sub-full state transition to draining
						shortageStartTime = lastT
					} else if levelBefore >= coreMaxCapacity {
						// Case: Full state transition to draining
						shortageStartTime = lastT
					}
				}

				// Invalidate potential end candidate if draining resumes before full charge
				potentialEndCandidate = nil

			} else {
				// Charging or Idle phase
				if shortageStartTime != nil {
					// Potential end of shortage sequence
					if potentialEndCandidate == nil {
						potentialEndCandidate = lastT
					}

					// Confirm end of shortage if full capacity is reached
					if nextLevel >= coreMaxCapacity {
						if let endT = potentialEndCandidate {
							let duration = endT - shortageStartTime!
							if endT >= measureStart {  // Only record relevant events
								maxShortageDuration = max(maxShortageDuration, duration)
							}
						}
						// Reset state
						shortageStartTime = nil
						potentialEndCandidate = nil
					}
				}
			}

			// Calculate stats strictly within overlap window, accounting for clamping
			if overlapDt > 0.000001 {
				if active == 0 {
					currentAllStoppedStreak += overlapDt
					maxAllStoppedDuration = max(maxAllStoppedDuration, currentAllStoppedStreak)
				} else {
					currentAllStoppedStreak = 0
				}

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
					// Just tracking outage time here if needed, but we use maxShortageDuration now.
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
					if levelAtWindowExit >= coreMaxCapacity {
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
			if active == 0 {
				currentAllStoppedStreak += dt
				maxAllStoppedDuration = max(maxAllStoppedDuration, currentAllStoppedStreak)
			} else {
				currentAllStoppedStreak = 0
			}

			if !minLevelInitialized {
				minLevel = currentLevel
				minLevelInitialized = true
			}

			let inputPower = Double(active) * battery.power
			let netPower = inputPower - requiredPower

			let possibleLevel = currentLevel + netPower * dt

			if computeLongestCycle
				&& currentLevel < coreMaxCapacity - 0.001
				&& possibleLevel >= coreMaxCapacity - 0.001
			{
				if netPower > 0.001 {
					let timeToFull = (coreMaxCapacity - currentLevel) / netPower
					let tFull = lastT + timeToFull
					if tFull >= measureStart && tFull <= measureEnd {
						fullChargeTimestamps.append(tFull)
					}
				}
			}

			if possibleLevel > coreMaxCapacity {
				totalOverflow += (possibleLevel - coreMaxCapacity)
				currentLevel = coreMaxCapacity
			} else if possibleLevel <= 0 {
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

	var longestFullChargeCycle: Double = 0
	if computeLongestCycle {
		if fullChargeTimestamps.count >= 2 {
			for i in 1..<fullChargeTimestamps.count {
				longestFullChargeCycle = max(
					longestFullChargeCycle,
					fullChargeTimestamps[i] - fullChargeTimestamps[i - 1]
				)
			}
		} else {
			longestFullChargeCycle = measurementDuration
		}
	}

	let wasteRateInBatteriesPerSec: Double =
		(overflowPerSec > 0) ? (overflowPerSec / oneBatteryTotalEnergy) : 0
	let netBenefitPerSec = saveRateInBatteriesPerSec - wasteRateInBatteriesPerSec

	let profile = OverlapProfile(
		overflowPerSecond: overflowPerSec,
		minBatteryLevel: minLevel,
		endBatteryLevel: currentLevel,
		hitFullCharge: hitFullCharge,
		netBenefitPerSecond: netBenefitPerSec,
		maxShortageDuration: maxShortageDuration
	)

	return OverlapStats(
		cycleTime: measurementDuration,
		overflow: totalOverflow,
		minLevel: minLevel,
		endLevel: currentLevel,
		maxAllStoppedDuration: maxAllStoppedDuration,
		profile: profile,
		longestFullChargeCycle: longestFullChargeCycle
	)
}

// --- Worst-case Splitter Search ---
func findWorstCaseOverlapStats(
	steps: [Step],
	battery: Config.Battery,
	actualBatteryCount: Int,
	requiredPower: Double,
	analyzedBatteryCount: Int,
	preSplit: Double,
	minOverflow: Double
) -> OverlapStats? {
	let stopToOutageSeconds = coreMaxCapacity / requiredPower
	let rootPeriod = Int((Double(beltSecondsPerTile) * preSplit).rounded())

	let uniqueStreamSets = enumerateUniqueStreamSets(steps: steps, rootPeriod: rootPeriod)

	// Further dedup by merged arrival pattern: stream sets producing
	// identical sorted arrival sequences behave identically in simulation.
	func dedupGCD(_ a: Int, _ b: Int) -> Int {
		let r = a % b
		return r == 0 ? b : dedupGCD(b, r)
	}
	func dedupLCM(_ a: Int, _ b: Int) -> Int {
		return (a * b) / dedupGCD(a, b)
	}

	var dedupedStreams: [[(period: Int, offset: Int)]] = []
	var seenArrivalKeys: Set<[Int]> = []

	for streams in uniqueStreamSets {
		guard !streams.isEmpty else {
			if seenArrivalKeys.insert([]).inserted {
				dedupedStreams.append([])
			}
			continue
		}
		var cycle = rootPeriod
		for s in streams { cycle = dedupLCM(cycle, s.period) }

		var arrivalTimes: [Int] = []
		for s in streams {
			let startPhase = ((s.offset % cycle) + cycle) % cycle
			var t = startPhase
			while t < cycle {
				arrivalTimes.append(t)
				t += s.period
			}
		}
		arrivalTimes.sort()

		if seenArrivalKeys.insert(arrivalTimes).inserted {
			dedupedStreams.append(streams)
		}
	}

	var worstStats: OverlapStats? = nil

	for streams in dedupedStreams {
		let stats = getOverlapStats(
			steps: steps,
			battery: battery,
			actualBatteryCount: actualBatteryCount,
			requiredPower: requiredPower,
			analyzedBatteryCount: analyzedBatteryCount,
			preSplit: preSplit,
			minOverflow: minOverflow,
			precomputedStreams: streams,
			computeLongestCycle: true
		)

		if !stats.profile.hitFullCharge { return nil }
		if stats.profile.minBatteryLevel <= 0 { return nil }
		if stats.profile.maxShortageDuration > maxShortageDurationLimitInSecond { return nil }
		if stopToOutageSeconds < stats.maxAllStoppedDuration { return nil }

		if let current = worstStats {
			if stats.longestFullChargeCycle > current.longestFullChargeCycle {
				worstStats = stats
			}
		} else {
			worstStats = stats
		}
	}

	return worstStats
}

// --- Analysis Helper ---
func analyzeSolutionOverlap(
	_ solution: Solution,
	battery: Config.Battery,
	batteryStatic: Config.Battery,
	worstCaseStats stats: OverlapStats
) {

	let oneBatteryTotalEnergyGlob = battery.totalEnergy
	let inputDoubleBatteryPowerGlob = battery.power * Double(solution.actualBatteryCount)
	let baselineBatteriesPerDayGlob =
		(inputDoubleBatteryPowerGlob * 86400) / oneBatteryTotalEnergyGlob
	let requiredBatteriesPerDayGlob =
		(solution.requiredPower * 86400) / oneBatteryTotalEnergyGlob
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
	let savedPerDay: Double = (netBenefit > 0) ? (netBenefit * 86400) : 0

	// print("\n==================================================")

	print("\t🛠 操作步骤(\(solution.totalStepCount)):　\(solution.allActions)")

	if stats.longestFullChargeCycle > 0 {
		print("\t🔄 最长周期:　\(formatDuration(stats.longestFullChargeCycle)) (最差分流下满电→最低→满电)")
	} else {
		print("\t🔄 最长周期:　无 (核心始终满电)")
	}
	print("\t📉 最低电量:　\(String(format: "%.4f", stats.minLevel))")
	print("\t📊 结束电量:　\(String(format: "%.4f", stats.endLevel))")
	// print("\t⏱ 停流间隔:　\(formatDuration(solution.requiredStopIntervalSeconds))")
	print("\t🔋 核心续航:　\(formatDuration(solution.stopToOutageSeconds)) (分流中断后还能坚持多久)")

	do {
		let durationStr = formatDuration(stats.maxAllStoppedDuration)
		let warning =
			stats.maxAllStoppedDuration > solution.stopToOutageSeconds ? "❌" : "⚠️"
		print(
			"\t\(warning) 最长完全亏电:　\(durationStr) (所有发电机同时停止)"
		)
	}

	do {
		let outageStr = formatDuration(stats.profile.maxShortageDuration)
		let warning =
			stats.profile.maxShortageDuration > maxShortageDurationLimitInSecond ? "❌" : "✅"
		print(
			"\t\(warning) 最长连续缺电:　\(outageStr) (限制: \(maxShortageDurationLimitInSecond)秒) (部分发电机同时停止)"
		)
	}
	print("\t🔌 最终功率:　\(String(format: "%.4f", solution.finalC))")
	print("\t⚖️ 差值:　　　\(String(format: "%.4f", solution.diff))")

	let oneBatteryTotalEnergy = battery.totalEnergy
	let inputBatteryPower = battery.power * Double(solution.actualBatteryCount)

	// 1. Calc waste rate (accounting for inevitable overflow if power > required)
	let effectiveOverflow = max(stats.profile.overflowPerSecond, solution.diff)
	let secondsToWasteOneBattery: Double =
		(effectiveOverflow > 0)
		? (oneBatteryTotalEnergy / effectiveOverflow) : Double.infinity

	// 2. Calc save time
	let excessPowerWithoutSplit = inputBatteryPower - solution.requiredPower
	let secondsToSaveOneBattery = oneBatteryTotalEnergy / excessPowerWithoutSplit

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
	let maxAllStoppedDuration: Double
	let profile: OverlapProfile
	let longestFullChargeCycle: Double
}

for config in configs {
	print("\n--------------------------------------------------")
	print("🔍 正在搜索:　\(config.name)")

	let batteryStatic = config.staticBattery
	let battery = config.analyzedBattery

	var currentBaseRequiredPower = config.baseRequiredPower
	// Loop to automatically increase power if no solution satisfies constraints
	while true {
		// Total required power (minus 200W core base)
		let totalPower = currentBaseRequiredPower - 200

		// Number of batteries to split
		var analyzedBatteryCount: Int = Int(ceil(totalPower / battery.power))

		let stopAtCount = max(1, min(minAnalyzedBatteryCount, analyzedBatteryCount))

		var solutions: [[Int: Int]: [Solution?]] = [:]
		var allKeptSolutions: [Solution] = []

		while analyzedBatteryCount >= stopAtCount {
			defer {
				analyzedBatteryCount -= 1
			}
			let maxAnalyzedBatteryDesignPower = battery.power * Double(analyzedBatteryCount)
			let maxAnalyzedBatteryPower = min(totalPower, maxAnalyzedBatteryDesignPower)
			let staticBatteryCount = Int(
				ceil((totalPower - maxAnalyzedBatteryPower) / batteryStatic.power))
			let requiredPower: Double =
				totalPower - Double(staticBatteryCount) * batteryStatic.power

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

					// Relaxed pruning threshold to accommodate high-surplus solutions
					guard diff <= 50 else {
						return
					}

					// Record solution. 'diff' sets minOverflow to penalize high-waste solutions.
					if diff >= allowedMinDiff, steps.last?.action == .add {
						let overlapStats = getOverlapStats(
							steps: steps,
							battery: battery,
							actualBatteryCount: actualBatteryCount,
							requiredPower: requiredPower,
							analyzedBatteryCount: analyzedBatteryCount,
							preSplit: preSplit,
							minOverflow: diff
						)
						let profile = overlapStats.profile

						// Must satisfy full charge reset (hard requirement)
						guard profile.hitFullCharge else {
							return
						}

						if profile.minBatteryLevel <= 0 {
							return
						}

						// Must satisfy max shortage duration limit
						if profile.maxShortageDuration > maxShortageDurationLimitInSecond {
							return
						}

						let stopToOutageSeconds = coreMaxCapacity / requiredPower

						// Must survive the max all stopped duration
						if stopToOutageSeconds < overlapStats.maxAllStoppedDuration {
							return
						}

						let requiredStopInterval =
							maxStopToOutageSeconds ?? overlapStats.maxAllStoppedDuration

						// Must survive the specified stop interval (or automatically calculated max stop duration)
						if stopToOutageSeconds < requiredStopInterval {
							return
						}

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
							depthLimit: depthLimit,
							stopToOutageSeconds: stopToOutageSeconds,
							requiredStopIntervalSeconds: requiredStopInterval
						)
						let index = steps.count
						if keepAllSolutions {
							allKeptSolutions.append(sol)
						} else {
							if let existing = solutions[preSplitBits]![index] {
								if isBetterSolution(sol, than: existing) {
									solutions[preSplitBits]![index] = sol
								}
							} else {
								solutions[preSplitBits]![index] = sol
							}
						}
					}

					// Pruning 2: Max possible power check (Upper bound assumption)
					let remaining = Double(depthLimit - steps.count)
					let maxPotential = sourceVal * (1.0 - pow(0.5, remaining))

					if testBattery + maxPotential < requiredPower {
						return
					}

					// Binary Split

					let half: Double = sourceVal / 2

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
					let third: Double = sourceVal / 3

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
					testBattery: 0,
					entropy: 0,
					n: 0,
					steps: [],
					values: []
				)
			}
		}

		var candidateSolutions: [Solution]
		if keepAllSolutions {
			candidateSolutions = allKeptSolutions
		} else {
			var uniqueSolutions: [String: Solution] = [:]
			for sol in solutions.values.flatMap({ $0 }).compactMap({ $0 }) {
				let key = sol.allActions
				if let existing = uniqueSolutions[key] {
					if isBetterSolution(sol, than: existing) {
						uniqueSolutions[key] = sol
					}
				} else {
					uniqueSolutions[key] = sol
				}
			}
			candidateSolutions = Array(uniqueSolutions.values)
		}

		var validatedTops: [(Solution, OverlapStats)] = []
		for sol in candidateSolutions {
			let preSplit = sol.preSplitBits.reduce(1.0) {
				$0 * pow(Double($1.key), Double($1.value))
			}
			guard
				let worstStats = findWorstCaseOverlapStats(
					steps: sol.steps,
					battery: battery,
					actualBatteryCount: sol.actualBatteryCount,
					requiredPower: sol.requiredPower,
					analyzedBatteryCount: sol.analyzedBatteryCount,
					preSplit: preSplit,
					minOverflow: sol.diff
				)
			else {
				continue
			}
			sol.overlap = worstStats.profile
			validatedTops.append((sol, worstStats))
		}

		validatedTops.sort { (a, b) in isBetterSolution(a.0, than: b.0) }
		if validatedTops.count > showTopSolutions {
			validatedTops = Array(validatedTops.prefix(showTopSolutions))
		}

		guard !validatedTops.isEmpty else {
			print("\t❌ 当前需求功率 \(currentBaseRequiredPower) 未找到符合条件的方案, 自动增加 5 功率重试...\n")
			currentBaseRequiredPower += 5
			continue
		}

		// Found solution, print and break inner loop to next config
		if currentBaseRequiredPower > config.baseRequiredPower {
			print("\t✅ 在增加到 \(currentBaseRequiredPower) 功率后找到方案 (原需求: \(config.baseRequiredPower))")
		}

		for (index, (sol, worstStats)) in validatedTops.enumerated() {
			if index > 0 {
				print("\n\t==================================================")
				print("\n")
			}
			analyzeSolutionOverlap(
				sol,
				battery: battery,
				batteryStatic: batteryStatic,
				worstCaseStats: worstStats
			)
		}
		break
	}  // end while true
}
