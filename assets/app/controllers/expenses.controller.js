define(['app/app', 'services', 'directives'], function(app) {
    expensesController.$inject = ['$scope', '$location', 'appState', 'getService', 'postService', 'deleteService', 'identityHandlerService'];
    function expensesController($scope, $location, appState, getService, postService, deleteService, identityHandlerService) {

        appState.error = appState.message = null;

        $scope.newItem = {};

        if(!appState.identity) {
            identityHandlerService().then(function(response) {
                $scope.newItem.organization = appState.current_organization.id;
                getService($scope, '/expense/', {organization:appState.current_organization.id}, 'expenses');
            });
        }
        else {
            $scope.newItem.organization = appState.current_organization.id;
            getService($scope, '/expense/', {organization:appState.current_organization.id}, 'expenses');
        }

        getService($scope, '/items/', {}, 'items');
        getService($scope, '/categories/', {}, 'categories');



        $scope.addItem = function() {
            postService('/items/', $scope.newItem)
                .then(function(response) {
                    getService($scope, '/items/', {}, 'items');
                    $scope.newItem = {user:appState.identity.id};
                    appState.message = "Item Added";
                });
        }

        $scope.remove = function(id) {
            alert(id);
            deleteService('/expense/'+id)
                .then(function(response) {
                    getService($scope, '/expense/', {organization:appState.current_organization.id}, 'items');
                    appState.message = "Expense Deleted";
                });
        }

    }

    app.register.controller('expensesController', expensesController);
})
