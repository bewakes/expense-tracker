define(['app/app'], function(app) {

    getService.$inject = ['$http', '$q', 'appState'];
    postService.$inject = ['$http', '$q', 'appState'];
    deleteService.$inject = ['$http', '$q', 'appState'];

    function getService($http, $q, appState) {
        return function(scope, url, params, scopeVar) {
            var deferred = $q.defer();
            params['organization'] = appState.current_organization.id;
            $http.get(url, {params:params})
                .then(
                    function(response) {
                        scope[scopeVar] = response.data;
                        deferred.resolve(response);
                    },
                    function(response) {
                        if(response.status == 403) {
                            //appState.error = "You are not permitted for action";
                        }else if (response.status== 400) {
                            // TODO: show errors
                            //appState.error = "Invalid field values";
                        } else {
                            //appState.error = "Request Could not be completed";
                        }
            });
            return deferred.promise;
        }
    }

    function postService($http, $q, appState) {
        return function(url, params) {
            appState.error = null;
            appState.message = null;
            var deferred = $q.defer();
            $http.post(url, params)
                .then(
                    deferred.resolve,
                    function(response) {
                        if(response.status== 403) {
                            appState.error = response.data.detail;
                        }else if (response.status== 400) {
                            // TODO: show errors
                            appState.error = JSON.stringify(response.data);//.detail?response.data.detail:"Invalid params";
                        } else {
                            appState.error = response.data.detail;
                            appState.error = "Error processing";
                        }
            });
            return deferred.promise;
        }
    }

    function putService($http, $q, appState) {
        return function(url, params) {
            appState.error = null;
            appState.message = null;
            var deferred = $q.defer();
            $http.put(url, params)
                .then(
                    deferred.resolve,
                    function(response) {
                        if(response.status== 403) {
                            appState.error = response.data.detail;
                        }else if (response.status== 400) {
                            // TODO: show errors
                            appState.error = JSON.stringify(response.data);//.detail?response.data.detail:"Invalid params";
                        } else {
                            appState.error = response.data.detail;
                            //appState.error = "Error processing";
                        }
            });
            return deferred.promise;
        }
    }

    function deleteService($http, $q, appState) {
        return function(url, params) {
            appState.error = null;
            appState.message = null;
            var deferred = $q.defer();
            $http.delete(url, {params:params})
                .then(
                    deferred.resolve,
                    function(response) {
                        if(response.status== 403) {
                            appState.error = response.data.detail;
                        }else if (response.status== 400) {
                            // TODO: show errors
                            appState.error = response.data.detail;
                        } else {
                            appState.error = response.data.detail;
                            //appState.error = "Error processing";
                        }
            });
            return deferred.promise;
        }
    }

    identityHandlerService.$inject = ['$http', 'appState', '$q'];
    function identityHandlerService($http, appState, $q) {
        return function() {
            if(!appState.identity) {
                var deferred = $q.defer();
                $http.get('/identity', {})
                    .then(function(response) {
                        appState.identity = response.data;
                        appState.current_organization = appState.identity.default_organization; // TODO: change this later
                        deferred.resolve();
                    }, function(response) {
                        window.location.hash = "";
                        window.location = "/";
                    });
                return deferred.promise;
            }
            else {
                var deferred = $q.defer();
                deferred.resolve();
                return deferred.promise;
            }
        }
    }

    httpService.$inject = ['getService', 'postService', 'putService'];
    function httpService(getService, postService, putService) {
        return {
            'get': getService,
            'post': postService,
            'put': putService
        }
    }


    // register services
    app.register.service('getService', getService);
    app.register.service('postService', postService);
    app.register.service('putService', putService);
    app.register.service('httpService', putService);
    app.register.service('deleteService', deleteService);
    app.register.service('identityHandlerService', identityHandlerService);

    lineChartService.$inject = ['getService'];
    function lineChartService(getService) {
        return function(scope, url, params, title) {
            getService({}, url, params, 'temp')
                .then(function(d){
                    scope.data = [{values:d.data.reverse(), key:'Daily Expense', color:'red'}];
                    var tickvals= [];
                    var labels = [];
                    for (var x=0;x<d.data.length;x++) { 
                        tickvals.push(x);
                        labels.push(d.data[x].date);
                    }
                    scope.options.chart.xAxis.tickValues = tickvals;
                    scope.options.chart.xAxis.tickFormat = function(d) { return labels[d]};
                });
            scope.options = {
                chart: {
                    type: 'lineChart',
                    height: 450,
                    margin : {
                        top: 20,
                        right: 20,
                        bottom: 40,
                        left: 55
                    },
                    x: function(d, i){ return i; },
                    y: function(d){ return d.total; },
                    useInteractiveGuideline: true,
                    dispatch: {
                        stateChange: function(e){ console.log("stateChange"); },
                        changeState: function(e){ console.log("changeState"); },
                        tooltipShow: function(e){ console.log("tooltipShow"); },
                        tooltipHide: function(e){ console.log("tooltipHide"); }
                    },
                    xAxis: {
                        axisLabel: 'Day'
                    },
                    yAxis: {
                        axisLabel: 'Expense',
                        tickFormat: function(d){
                            return d3.format('.02f')(d);
                        },
                        axisLabelDistance: -10
                    },
                    callback: function(chart){
                        console.log("!!! lineChart callback !!!");
                    }
                },
                title: {
                    enable: true,
                    text: title || ""
                }
            };
        }
    }

    app.register.service('lineChartService', lineChartService);
});
