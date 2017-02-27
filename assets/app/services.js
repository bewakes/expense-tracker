define(['app/app'], function(app) {

    getService.$inject = ['$http', '$q', 'appState'];
    postService.$inject = ['$http', '$q', 'appState'];

    function getService($http, $q, appState) {
        return function(scope, url, params, scopeVar) {
            var deferred = $q.defer();
            $http.get(url, params)
                .then(
                    function(response) {
                        scope[scopeVar] = response.data;
                        deferred.resolve(response);
                    },
                    function(response) {
                        if(response.status == 403) {
                            appState.error = "You are not permitted for action";
                        }else if (response.status== 400) {
                            // TODO: show errors
                            appState.error = "Invalid field values";
                        } else {
                            appState.error = "Request Could not be completed";
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
                            appState.error = "Invalid field values";
                        } else {
                            appState.error = response.data.detail;
                            appState.error = "Error processing";
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
            $http.delete(url, params)
                .then(
                    deferred.resolve,
                    function(response) {
                        if(response.status== 403) {
                            appState.error = response.data.detail;
                        }else if (response.status== 400) {
                            // TODO: show errors
                            appState.error = "Invalid field values";
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
                        deferred.resolve();
                    }, function(response) {
                        window.location.hash = "";
                        window.location = "/";
                    });
                return deferred.promise;
            }
            else {
                var deferred = $q.defer();
                return deferred.promise;
            }
        }
    }

    // register services
    app.register.service('getService', getService);
    app.register.service('postService', postService);
    app.register.service('deleteService', deleteService);
    app.register.service('identityHandlerService', identityHandlerService);
});
