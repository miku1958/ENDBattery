import Foundation

/// Decoded calculator input. Mirrors the editable sections of the original
/// hardcoded driver: a list of scenarios plus the tunable parameters. Every
/// tunable field is optional in JSON and falls back to the documented default.
public struct CalculatorInput: Decodable {
	var configs: [Config]
	var minAnalyzedBatteryCount: Int
	var extraBeltInSteps: Int
	var maxDepthLimit: Int
	var showTopSolutions: Int
	var allowedMinDiff: Double
	var enableThree: Bool
	var safetyThreshold: Double
	var maxStopToOutageSeconds: Double?
	var maxShortageDurationLimitInSecond: Double
	var keepAllSolutions: Bool

	private enum CodingKeys: String, CodingKey {
		case configs, minAnalyzedBatteryCount, extraBeltInSteps, maxDepthLimit,
			showTopSolutions, allowedMinDiff, enableThree, safetyThreshold,
			maxStopToOutageSeconds, maxShortageDurationLimitInSecond, keepAllSolutions
	}

	public init(from decoder: Decoder) throws {
		let c = try decoder.container(keyedBy: CodingKeys.self)
		configs = try c.decode([Config].self, forKey: .configs)
		minAnalyzedBatteryCount = try c.decodeIfPresent(Int.self, forKey: .minAnalyzedBatteryCount) ?? 1
		extraBeltInSteps = try c.decodeIfPresent(Int.self, forKey: .extraBeltInSteps) ?? 1
		maxDepthLimit = try c.decodeIfPresent(Int.self, forKey: .maxDepthLimit) ?? 9
		showTopSolutions = try c.decodeIfPresent(Int.self, forKey: .showTopSolutions) ?? 1
		allowedMinDiff = try c.decodeIfPresent(Double.self, forKey: .allowedMinDiff) ?? 10
		enableThree = try c.decodeIfPresent(Bool.self, forKey: .enableThree) ?? true
		safetyThreshold = try c.decodeIfPresent(Double.self, forKey: .safetyThreshold) ?? 0.15
		maxStopToOutageSeconds = try c.decodeIfPresent(Double.self, forKey: .maxStopToOutageSeconds)
		maxShortageDurationLimitInSecond = try c.decodeIfPresent(Double.self, forKey: .maxShortageDurationLimitInSecond) ?? 1000
		keepAllSolutions = try c.decodeIfPresent(Bool.self, forKey: .keepAllSolutions) ?? false
	}
}

/// Copy the per-run tunables from the input into the module-level state the
/// search functions read. Faithful to the original file-level globals.
func applyOptions(_ input: CalculatorInput) {
	minAnalyzedBatteryCount = input.minAnalyzedBatteryCount
	extraBeltInSteps = input.extraBeltInSteps
	maxDepthLimit = input.maxDepthLimit
	showTopSolutions = input.showTopSolutions
	allowedMinDiff = input.allowedMinDiff
	enableThree = input.enableThree
	safetyThreshold = input.safetyThreshold
	maxStopToOutageSeconds = input.maxStopToOutageSeconds
	maxShortageDurationLimitInSecond = input.maxShortageDurationLimitInSecond
	keepAllSolutions = input.keepAllSolutions
}

extension Config: Decodable {
	private enum CodingKeys: String, CodingKey {
		case name, staticBattery, analyzedBattery, baseRequiredPower
	}

	init(from decoder: Decoder) throws {
		let c = try decoder.container(keyedBy: CodingKeys.self)
		let name = try c.decode(String.self, forKey: .name)
		let staticBattery = try c.decodeIfPresent([Config.Battery].self, forKey: .staticBattery) ?? []
		let analyzedBattery = try c.decode(Config.Battery.self, forKey: .analyzedBattery)
		let baseRequiredPower = try c.decode(Double.self, forKey: .baseRequiredPower)
		self.init(
			name: name,
			staticBattery: staticBattery,
			analyzedBattery: analyzedBattery,
			baseRequiredPower: baseRequiredPower
		)
	}
}

extension Config.Battery: Decodable {
	private enum CodingKeys: String, CodingKey { case type, count }

	private enum Kind: String, Decodable {
		case originium, green, blue, purple, lowEarth, midEarth
	}

	init(from decoder: Decoder) throws {
		let c = try decoder.container(keyedBy: CodingKeys.self)
		let kind = try c.decode(Kind.self, forKey: .type)
		let count = try c.decodeIfPresent(Int.self, forKey: .count)
		switch kind {
		case .originium: self = .originium(count: count)
		case .green: self = .green(count: count)
		case .blue: self = .blue(count: count)
		case .purple: self = .purple(count: count)
		case .lowEarth: self = .lowEarth(count: count)
		case .midEarth: self = .midEarth(count: count)
		}
	}
}
