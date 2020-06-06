
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

interface TaskDescr {
    descr: string;
    uuid: string;
    time_elapsed?: number;
    estimated_completion?: number;
}
interface TaskStatus {
    active: Array<TaskDescr>;
    waiting: Array<TaskDescr>;
}

// update the 'dyn-status' object
async function updateTasks() {
    const targetNode = document.getElementById('dyn-status');
    if (targetNode) {
        const st = await fetch('/api/tasks_status/');
        const st_json = <TaskStatus> await st.json();
        var s = '';

        for (let j of st_json.active) {
            var compl = "";
            if (j.estimated_completion) {
                compl = `, estimated completion: ${j.estimated_completion}%`;
            }
            s += `<li class="list-group-item">
                <div class="spinner-border"></div>
                <p>active task: (uuid: ${j.uuid}, elapsed: ${(j.time_elapsed||0) / 1000}s${compl})</p>
                <pre>${j.descr}</pre>
                <form id="cancel" action="/interrupt/" method="POST">
                 <button class="btn btn-warning"> interrupt </button>
                </form> </li>`;
        }

        for (let j of st_json.waiting) {
            s += `<li class="list-group-item">
                <div class="spinner-border"></div>
                <p>waiting task (uuid ${j.uuid})</p>
                <pre>${j.descr}</pre>
                </li>`;
        }

        targetNode.innerHTML = s;
    }
}

lazyLoadAll();
document.addEventListener('change', () => {
    lazyLoadAll();
});

window.onload = () => {
    if (document.getElementById('dyn-status') !== undefined) {
        updateTasks();
        setInterval(updateTasks, 500);
    }
}
