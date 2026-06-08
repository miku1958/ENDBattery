import Foundation
import XCTest

@testable import ENDBatteryCore

#if canImport(Darwin)
	import Darwin
#elseif canImport(Glibc)
	import Glibc
#endif

/// Capture everything `body` writes to stdout. Output of a single scenario is a
/// few hundred bytes — well under the OS pipe buffer — so a synchronous read
/// after the body finishes cannot deadlock.
private func captureStdout(_ body: () -> Void) -> String {
	fflush(stdout)
	let saved = dup(fileno(stdout))
	let pipe = Pipe()
	dup2(pipe.fileHandleForWriting.fileDescriptor, fileno(stdout))

	body()

	fflush(stdout)
	dup2(saved, fileno(stdout))
	close(saved)
	pipe.fileHandleForWriting.closeFile()

	let data = pipe.fileHandleForReading.readDataToEndOfFile()
	return String(data: data, encoding: .utf8) ?? ""
}

private func report(forJSON json: String) -> String {
	let input = try! JSONDecoder().decode(CalculatorInput.self, from: Data(json.utf8))
	return captureStdout { runCalculation(input: input) }
}

final class CalculatorTests: XCTestCase {
	func test4号谷地() {
		let out = report(forJSON:
			"""
			{
				"configs": [
					{
						"name": "4号谷地",
						"staticBattery": [{ "type": "purple" }],
						"analyzedBattery": { "type": "purple" },
						"baseRequiredPower": 5230
					}
				]
			}
			""")

		XCTAssertTrue(out.contains("📦 电池数量:　高容谷地电池(常开): 3个, 高容谷地电池(分流): 2个"), out)
		XCTAssertTrue(out.contains("🛠 操作步骤(8):　3🔴×2     2🟢     3🟢     2🔴     2🟢     3🔴     3🟢"), out)
		XCTAssertTrue(out.contains("🔌 最终功率:　1742.7984"), out)
		XCTAssertTrue(out.contains("⚖️ 差值:　　　12.7984"), out)
		XCTAssertTrue(
			out.contains("💎 净收益:　　0.010391 颗/秒  ≈  897.778颗/天(理论可省: 922.909 颗/天, 差值: 25.131)"), out)
	}

	func test武陵() {
		let out = report(forJSON:
			"""
			{
				"configs": [
					{
						"name": "武陵",
						"staticBattery": [{ "type": "midEarth", "count": 1 }],
						"analyzedBattery": { "type": "purple" },
						"baseRequiredPower": 6210
					}
				]
			}
			""")

		XCTAssertTrue(out.contains("📦 电池数量:　中容武陵电池: 1个, 高容谷地电池(常开): 2个, 高容谷地电池(分流): 1个"), out)
		XCTAssertTrue(out.contains("🛠 操作步骤(9):　3🔴×2     2🔴     2🟢     3🔴×3     3🟢×2"), out)
		XCTAssertTrue(out.contains("🔌 最终功率:　621.1706"), out)
		XCTAssertTrue(out.contains("⚖️ 差值:　　　11.1706"), out)
		XCTAssertTrue(
			out.contains("💎 净收益:　　0.010882 颗/秒  ≈  940.247颗/天(理论可省: 962.182 颗/天, 差值: 21.935)"), out)
	}

	/// Omitted tunables fall back to defaults, so a bare config still solves.
	func testDefaultsApplyWhenTunablesOmitted() {
		let out = report(forJSON:
			"""
			{ "configs": [ { "name": "4号谷地", "staticBattery": [{ "type": "purple" }], "analyzedBattery": { "type": "purple" }, "baseRequiredPower": 5230 } ] }
			""")
		XCTAssertTrue(out.contains("🔍 正在搜索:　4号谷地"), out)
		XCTAssertTrue(out.contains("📦 电池数量:　高容谷地电池(常开): 3个, 高容谷地电池(分流): 2个"), out)
	}
}
