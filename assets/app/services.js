define(['app/app'], function(app) {

    getService.$inject = ['$http', '$q', 'appState'];
    postService.$inject = ['$http', '$q', 'appState'];

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
});
