
function lazyLoad(e: HTMLElement, target: string) {
    //return h('h2', null, `lazy load target=${target}`);
    console.log(`fetch ${target}`);
    fetch(target)
        .then(res => res.text())
        .then(body => {
            console.log(`got body for ${target}`);
            e.innerHTML = body;
        });
}

function lazyLoadAll() {
    console.log('lazy load all');
    document.querySelectorAll<HTMLElement>('.lazy-load').forEach(e => {
      let target = <string> e.getAttribute('x_src');
      lazyLoad(e, target);
    });
}

lazyLoadAll();
document.addEventListener('change', () => {
    lazyLoadAll();
});
