exports.timed = function(label){
  return function(thunk){
    const t0 = performance.now();
    const v = thunk(null);
    const t1 = performance.now();
    console.log("%s: %oms", label, t1 - t0);
    return v;
  }
}
