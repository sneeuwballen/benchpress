function lazyLoad(e, target) {
    //return h('h2', null, `lazy load target=${target}`);
    console.log("fetch " + target);
    fetch(target)
        .then(function (res) { return res.text(); })
        .then(function (body) {
        console.log("got body for " + target);
        e.innerHTML = body;
    });
}
function lazyLoadAll() {
    console.log('lazy load all');
    document.querySelectorAll('.lazy-load').forEach(function (e) {
        var target = e.getAttribute('x_src');
        lazyLoad(e, target);
    });
}
lazyLoadAll();
document.addEventListener('change', function () {
    lazyLoadAll();
});
