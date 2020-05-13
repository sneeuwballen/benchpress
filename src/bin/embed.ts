
// Load a link lazily
async function lazyLoad(e: HTMLElement, target: string) {
    //return h('h2', null, `lazy load target=${target}`);
    console.log(`fetch ${target}`);
    e.innerHTML = `<div class="spinner-border spinner-border-sm">
        <span class="sr-only">loading</span>
        </div>` + e.innerHTML;
    const res = await fetch(target);
    const body = await res.text();
    console.log(`got body for ${target}`);
    if (res.ok) {
        const e2 = document.createElement('div');
        e2.innerHTML = body;
        e.replaceWith(e2);
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

// update the 'dyn-status' object
async function updateTasks() {
    const targetNode =document.getElementById('dyn-status');
    if (targetNode) {
        const st = await fetch('/api/tasks_status/');
        const st_json = await st.json();
        var s = '';
        let n1 = document.getElementById('dyn-status.n-in-q');
        s += `<li class="list-group-item">jobs in queue: ${st_json.in_queue} </li>`;

        if (st_json.cur_job) {
            s += `<li class="list-group-item">
                <div class="spinner-border"></div>
                <pre>current task: (${st_json.cur_job.elapsed}s)
                 ${st_json.cur_job.task}</pre>
                <form id="cancel" action="/interrupt/" method="POST">
                 <button class="btn btn-warning"> interrupt </button>
                </form> </li>`;
        }

        targetNode.innerHTML = s;
    }
}

setInterval(updateTasks, 500);

document.addEventListener('turbolinks:load', () => {
    lazyLoadAll();
    updateTasks();
});
