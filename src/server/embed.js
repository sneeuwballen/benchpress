"use strict";
var __awaiter = (this && this.__awaiter) || function (thisArg, _arguments, P, generator) {
    function adopt(value) { return value instanceof P ? value : new P(function (resolve) { resolve(value); }); }
    return new (P || (P = Promise))(function (resolve, reject) {
        function fulfilled(value) { try { step(generator.next(value)); } catch (e) { reject(e); } }
        function rejected(value) { try { step(generator["throw"](value)); } catch (e) { reject(e); } }
        function step(result) { result.done ? resolve(result.value) : adopt(result.value).then(fulfilled, rejected); }
        step((generator = generator.apply(thisArg, _arguments || [])).next());
    });
};
var __generator = (this && this.__generator) || function (thisArg, body) {
    var _ = { label: 0, sent: function() { if (t[0] & 1) throw t[1]; return t[1]; }, trys: [], ops: [] }, f, y, t, g;
    return g = { next: verb(0), "throw": verb(1), "return": verb(2) }, typeof Symbol === "function" && (g[Symbol.iterator] = function() { return this; }), g;
    function verb(n) { return function (v) { return step([n, v]); }; }
    function step(op) {
        if (f) throw new TypeError("Generator is already executing.");
        while (_) try {
            if (f = 1, y && (t = op[0] & 2 ? y["return"] : op[0] ? y["throw"] || ((t = y["return"]) && t.call(y), 0) : y.next) && !(t = t.call(y, op[1])).done) return t;
            if (y = 0, t) op = [op[0] & 2, t.value];
            switch (op[0]) {
                case 0: case 1: t = op; break;
                case 4: _.label++; return { value: op[1], done: false };
                case 5: _.label++; y = op[1]; op = [0]; continue;
                case 7: op = _.ops.pop(); _.trys.pop(); continue;
                default:
                    if (!(t = _.trys, t = t.length > 0 && t[t.length - 1]) && (op[0] === 6 || op[0] === 2)) { _ = 0; continue; }
                    if (op[0] === 3 && (!t || (op[1] > t[0] && op[1] < t[3]))) { _.label = op[1]; break; }
                    if (op[0] === 6 && _.label < t[1]) { _.label = t[1]; t = op; break; }
                    if (t && _.label < t[2]) { _.label = t[2]; _.ops.push(op); break; }
                    if (t[2]) _.ops.pop();
                    _.trys.pop(); continue;
            }
            op = body.call(thisArg, _);
        } catch (e) { op = [6, e]; y = 0; } finally { f = t = 0; }
        if (op[0] & 5) throw op[1]; return { value: op[0] ? op[1] : void 0, done: true };
    }
};
// Load a link lazily
function lazyLoad(e, target) {
    return __awaiter(this, void 0, void 0, function () {
        var res, body, e2;
        return __generator(this, function (_a) {
            switch (_a.label) {
                case 0:
                    //return h('h2', null, `lazy load target=${target}`);
                    console.log("fetch " + target);
                    e.innerHTML = "<div class=\"spinner-border spinner-border-sm\">\n        <span class=\"sr-only\">loading</span>\n        </div>" + e.innerHTML;
                    return [4 /*yield*/, fetch(target)];
                case 1:
                    res = _a.sent();
                    return [4 /*yield*/, res.text()];
                case 2:
                    body = _a.sent();
                    console.log("got body for " + target);
                    if (res.ok) {
                        e2 = document.createElement('div');
                        e2.innerHTML = body;
                        e.replaceWith(e2);
                    }
                    else {
                        e.innerHTML = "<div class=\"alert alert-danger\">\n            error " + res.status + "\n            <ul class=\"container-list\">\n                <li class=\"container-list-item\">\n                    <p>context: lazy loading of " + target + " </p>\n                </li>\n                <li class=\"container-list-item\">\n                    <p>error: " + body + " </p>\n                </li>\n            </ul>\n        </div>";
                    }
                    return [2 /*return*/];
            }
        });
    });
}
function lazyLoadAll() {
    console.log('lazy load all');
    document.querySelectorAll('.lazy-load').forEach(function (e) {
        var target = e.getAttribute('x_src');
        lazyLoad(e, target);
    });
}
// update the 'dyn-status' object
function updateTasks() {
    return __awaiter(this, void 0, void 0, function () {
        var targetNode, st, st_json, s, n1;
        return __generator(this, function (_a) {
            switch (_a.label) {
                case 0:
                    targetNode = document.getElementById('dyn-status');
                    if (!targetNode) return [3 /*break*/, 3];
                    return [4 /*yield*/, fetch('/api/tasks_status/')];
                case 1:
                    st = _a.sent();
                    return [4 /*yield*/, st.json()];
                case 2:
                    st_json = _a.sent();
                    s = '';
                    n1 = document.getElementById('dyn-status.n-in-q');
                    s += "<li class=\"list-group-item\">jobs in queue: " + st_json.in_queue + " </li>";
                    if (st_json.cur_job) {
                        s += "<li class=\"list-group-item\">\n                <div class=\"spinner-border\"></div>\n                <pre>current task: (" + st_json.cur_job.elapsed + "s)\n                 " + st_json.cur_job.task + "</pre>\n                <form id=\"cancel\" action=\"/interrupt/\" method=\"POST\">\n                 <button class=\"btn btn-warning\"> interrupt </button>\n                </form> </li>";
                    }
                    targetNode.innerHTML = s;
                    _a.label = 3;
                case 3: return [2 /*return*/];
            }
        });
    });
}
lazyLoadAll();
document.addEventListener('change', function () {
    lazyLoadAll();
});
window.onload = function () {
    if (document.getElementById('dyn-status') !== undefined) {
        updateTasks;
        setInterval(updateTasks, 500);
    }
};
