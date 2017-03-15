define(['app/app', 'services', 'directives'], function(app) {
    settingsController.$inject = ['$location', '$scope', 'appState', 'getService', 'postService', 'deleteService', 'identityHandlerService', 'putService', '$anchorScroll'];

    function settingsController($location, $scope, appState, getService, postService, deleteService, identityHandlerService, putService, $anchorScroll) {

        appState.error = appState.message = null;

        if(!appState.identity) {
            identityHandlerService().then(function(response) {
                $scope.reload();
                $scope.currentUser = appState.identity;
            });
        }
        else {
            $scope.reload();
        }

        $scope.edit = false;

        $scope.searchUserList = [];

        $scope.reload = function() {
            $scope.updateOrg = angular.copy(appState.current_organization);
            getService($scope, '/orgusers/', {organization:appState.current_organization.id}, 'orgUsers');
        }

        $scope.setEditMode = function() {
            $scope.edit = true;
        }

        $scope.cancelEdit = function() {
            $scope.edit = false;
            $scope.updateOrg = angular.copy(appState.current_organization);
        }

        $scope.update = function() {
            putService('/organizations/'+appState.current_organization.id+'/', $scope.updateOrg)
                .then(function(d){
                    identityHandlerService().then(function(){
                        $scope.reload();
                        $scope.edit = false;
                    });
                });
        }

    }

    app.register.controller('settingsController', settingsController);

});
