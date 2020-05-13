
async function lazyLoad(e: HTMLElement, target: string) {
    //return h('h2', null, `lazy load target=${target}`);
    console.log(`fetch ${target}`);
    e.innerHTML = `<div class="spinner-border">
        <span class="sr-only">loading</span>
        </div>`;
    const res = await fetch(target);
    const body = await res.text();
    console.log(`got body for ${target}`);
    if (res.ok) {
        e.innerHTML = body;
    } else {
        e.innerHTML = `<div class="alert alert-danger">
            error ${res.status}
            <ul class="container-list">
                <li class="container-list-item">
                    <p>context: lazy loading of ${target} </p>
                </li>
                <li class="container-list-item">
                    <p>error: ${body} </p>
                </li>
            </ul>
        </div>`;
    }
}

function lazyLoadAll() {
    console.log('lazy load all');
    document.querySelectorAll<HTMLElement>('.lazy-load').forEach(e => {
      const target = <string> e.getAttribute('x_src');
      lazyLoad(e, target);
    });
}

lazyLoadAll();
document.addEventListener('change', () => {
    lazyLoadAll();
});
