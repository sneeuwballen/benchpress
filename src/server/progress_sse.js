// rsjs: live progress via SSE
// Use: <div data-sse-progress data-sse-href="/api/ext-jobs-status/" data-sse-swap="innerHTML">
(function () {
    function connectSse(el) {
        var href = el.getAttribute('data-sse-href') || '/api/ext-jobs-status/';
        var swap = el.getAttribute('data-sse-swap') || 'innerHTML';
        var target = '#' + el.id;
        var es = new EventSource('/progress/sse');
        es.addEventListener('progress-refresh', function () {
            htmx.ajax('GET', href, { target: target, swap: swap });
        });
        es.addEventListener('progress-update', function (e) {
            var uuid = e.data;
            if (!uuid) return;
            // Dispatch a custom DOM event so HTMX can trigger per-job reloads
            document.body.dispatchEvent(new CustomEvent('job-update-' + uuid));
        });
    }
    if (document.readyState !== 'loading') {
        document.querySelectorAll('[data-sse-progress]').forEach(connectSse);
    } else {
        document.addEventListener('DOMContentLoaded', function () {
            document.querySelectorAll('[data-sse-progress]').forEach(connectSse);
        });
    }
    // Re-connect after htmx swaps (new elements may appear)
    document.addEventListener('htmx:afterSwap', function () {
        document.querySelectorAll('[data-sse-progress]:not([data-sse-initialised])').forEach(function (el) {
            el.setAttribute('data-sse-initialised', '');
            connectSse(el);
        });
    });
})();
