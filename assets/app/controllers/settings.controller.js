define(['app/app', 'services', 'directives'], function(app) {
    settingsController.$inject = ['$location', '$scope', 'appState', 'getService', 'postService', 'deleteService', 'identityHandlerService', 'putService', '$anchorScroll'];

    function settingsController($location, $scope, appState, getService, postService, deleteService, identityHandlerService, putService, $anchorScroll) {

        appState.error = appState.message = null;

        $scope.reload = function() {
            $scope.updateOrg = angular.copy(appState.current_organization);
            getService($scope, '/orgusers/', {organization:appState.current_organization.id}, 'orgUsers');
            $scope.edit = $scope.editUser = $scope.addOrg= false;
            $scope.orgs = appState.identity.organizations;
            $scope.newOrg = {name:''};
        }

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
        $scope.editUsers = false;

        $scope.searchUserList = [];

        $scope.loadUsers = function() {
            if ($scope.searchText.length>=3) {
                getService($scope, '/users/', {query:$scope.searchText}, 'searchUserList');
            }
        }

        $scope.addUser = function(user) {
            postService('/expenses/adduser/', {user:user.id, organization:appState.current_organization.id})
                .then(function(d) {
                    $scope.reload();
                });
        }

        $scope.addNewOrg = function() {
            postService('/organizations/', $scope.newOrg)
                .then(function(d) {
                    location.reload();
                });
        }

        $scope.removeUser = function(user) {
            postService('/expenses/removeuser/', {user:user.id, organization:appState.current_organization.id})
                .then(function(d) {
                    $scope.reload();
                });
        }

        $scope.setEditMode = function() {
            $scope.edit = true;
        }

        $scope.cancelEdit = function() {
            $scope.edit = false;
            $scope.updateOrg = angular.copy(appState.current_organization);
        }
        $scope.cancelEditUser = function() {
            $scope.editUser = false;
        }

        $scope.update = function() {
            var name = $scope.updateOrg.name;
            putService('/organizations/'+appState.current_organization.id+'/', $scope.updateOrg)
                .then(function(d){
                    identityHandlerService().then(function(){
                        location.reload();
                    });
                });
        }

    }

    app.register.controller('settingsController', settingsController);

});
