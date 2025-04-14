/**
 * Example of using gan-web-bluetooth library in the pure Node.js environment
 */

const bluetooth = require('webbluetooth');
const gan = require('gan-web-bluetooth');

// Put Web Bluetooth API implementation in the same namespace like in browser environment
globalThis.navigator = {
    bluetooth: new bluetooth.Bluetooth({
        deviceFound: onDeviceFound,
        scanTime: 30
    })
};

var deviceList = [];
// Callback invoked during Bluetooth scanning procedure on each new device found
function onDeviceFound(device, selectFn) {
    deviceList.push({
        device: device,
        select: selectFn
    });
    console.log(`${deviceList.length}) ${device.name}`);
}

// Read user input from console
const readline = require('readline').createInterface({
    input: process.stdin,
    output: process.stdout,
    terminal: false
});
console.log(`=> Scanning for bluetooth devices, enter device number to connect:`);
readline.on('line', (input) => {
    var idx = parseInt(input) - 1;
    if (deviceList[idx]) {
        readline.close();
        console.log(`Connecting to ${deviceList[idx].device.name}`);
        deviceList[idx].select();
    } else {
        console.log(`Invalid device number entered!`);
    }
});

// Retrieve cube device MAC address using implementation-specific fields containing advertising packet data from device
async function customMacAddressProvider(device) {
    var mac = [];
    var mfData = [...device._adData.manufacturerData.values()][0];
    if (mfData && mfData.byteLength >= 9) {
        for (let i = 8; i > 2; i--) {
            mac.push(mfData.getUint8(i).toString(16).toUpperCase().padStart(2, "0"));
        }
    }
    return mac.join(':');
}

// Connect to the cube and subscribe to the events
async function main() {

    var conn = await gan.connectGanCube(customMacAddressProvider);
    console.log(`Connected successfully`);

    conn.events$.subscribe((cubeEvent) => {
        if (cubeEvent.type != "GYRO") {
            console.log('GanCubeEvent', cubeEvent);
        }
    });

    await conn.sendCubeCommand({ type: "REQUEST_HARDWARE" });
    await conn.sendCubeCommand({ type: "REQUEST_FACELETS" });
    await conn.sendCubeCommand({ type: "REQUEST_BATTERY" });

}

main();
// Avoid Node.js process termination due to empty event loop queue
setInterval(() => { }, 1 << 30);

