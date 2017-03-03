define(['app/app', 'services', 'directives'], function(app) {
    settingsController.$inject = ['$location', '$scope', 'appState', 'getService', 'postService', 'deleteService', 'identityHandlerService', 'putService', '$anchorScroll'];

    function settingsController($location, $scope, appState, getService, postService, deleteService, identityHandlerService, putService, $anchorScroll) {

        appState.error = appState.message = null;

        if(!appState.identity) {
            identityHandlerService().then(function(response) {
                $scope.reload();
            });
        }
        else {
            $scope.reload();
        }

        $scope.edit = false;

        //$scope.searchUserList = ["bibek", "pandey"];
        $scope.reload = function() {
            $scope.updateOrg = angular.copy(appState.current_organization);
        }

        $scope.setEditMode = function() {
            $scope.edit = true;
        }

        $scope.cancelEdit = function() {
            $scope.edit = false;
        }

    }

    app.register.controller('settingsController', settingsController);

});
