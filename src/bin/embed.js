var lazyLoad = function (e, target) {
    //return h('h2', null, `lazy load target=${target}`);
    console.log("fetch " + target);
    fetch(target)
        .then(function (res) { return res.text(); })
        .then(function (body) {
        console.log("got body");
        e.innerHTML = body;
    });
};
document.querySelectorAll('.lazy-load').forEach(function (e) {
    var target = e.getAttribute('x_src');
    lazyLoad(e, target);
});
