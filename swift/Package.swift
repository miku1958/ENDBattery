// swift-tools-version: 5.4
// The swift-tools-version declares the minimum version of Swift required to build this package.

import PackageDescription

let package = Package(
    name: "ENDBattery",
    targets: [
        .target(
            name: "ENDBatteryCore"
        ),
        .executableTarget(
            name: "ENDBattery",
            dependencies: ["ENDBatteryCore"]
        ),
        .testTarget(
            name: "ENDBatteryCoreTests",
            dependencies: ["ENDBatteryCore"]
        ),
    ]
)
