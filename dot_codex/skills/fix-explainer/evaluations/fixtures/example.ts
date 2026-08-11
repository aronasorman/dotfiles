const changedPaths = getChangedPaths();
const services = discoverServices(changedPaths);
console.log(JSON.stringify({ services }));
