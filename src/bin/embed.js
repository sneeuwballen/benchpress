
// update the 'dyn-status' object
function updateTasks() {
 fetch('/api/tasks_status/').then(x => {
  return x.json();
 }).then(x => {
  var s = '';
  let n1 = document.getElementById('dyn-status.n-in-q');
  s += `<li class="list-group-item">jobs in queue: ${x.in_queue} </li>`;

  if (x.cur_job) {
   s += `<li class="list-group-item">
   <div class="spinner-border"></div>
    <pre>current task: (${x.cur_job.elapsed}s)
     ${x.cur_job.task}</pre>
    <form id="cancel" action="/interrupt/" method="POST">
     <button class="btn btn-warning"> interrupt </button>
    </form> </li>`;
  }

  document.getElementById('dyn-status').innerHTML = s;
 });
}

/*
document.addEventListener("turbolinks:load", () => {
  alert('turbolinks!');
});
*/

//setInterval(updateTasks, 1500);
window.onload = () => {
  Turbolinks.setProgressBarDelay(100);
  //updateTasks();
  //
};

