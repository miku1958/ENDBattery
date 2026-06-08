import ENDBatteryCore
import Foundation

// WASM entry point: read a JSON CalculatorInput from stdin, run the search,
// print the report. Local verification and debugging go through `swift test`.

let startTime = Date()

let inputData = FileHandle.standardInput.readDataToEndOfFile()

do {
	let input = try JSONDecoder().decode(CalculatorInput.self, from: inputData)
	runCalculation(input: input)
} catch {
	let message = "❌ 解析输入 JSON 失败: \(error)"
	FileHandle.standardError.write(Data((message + "\n").utf8))
	print(message)
}

let elapsed = Date().timeIntervalSince(startTime)
print(String(format: "\n🕐 总耗时: %.2f 秒", elapsed))
