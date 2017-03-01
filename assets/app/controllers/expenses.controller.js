define(['app/app', 'services', 'directives'], function(app) {
    expensesController.$inject = ['$scope', '$location', 'appState', 'getService', 'postService', 'deleteService', 'identityHandlerService'];
    function expensesController($scope, $location, appState, getService, postService, deleteService, identityHandlerService) {

        appState.error = appState.message = null;

        $scope.newExpense = {date:new Date(), description:''};

        if(!appState.identity) {
            identityHandlerService().then(function(response) {
                getService($scope, '/expense/', {organization:appState.current_organization.id}, 'expenses');
            });
        }
        else {
            getService($scope, '/expense/', {organization:appState.current_organization.id}, 'expenses');
        }

        getService($scope, '/items/', {}, 'items');
        getService($scope, '/categories/', {}, 'categories');



        $scope.addExpense= function() {
            postService('/expense/', $scope.newExpense)
                .then(function(response) {
                    getService($scope, '/expense/', {organization:appState.current_organization.id}, 'expenses');
                    $scope.newExpense = {date:new Date(), description:''};
                    appState.message = "Expense Added";
                });
        }

        $scope.remove = function(id) {
            deleteService('/expense/'+id, {organization:appState.current_organization.id})
                .then(function(response) {
                    getService($scope, '/expense/', {organization:appState.current_organization.id}, 'expenses');
                    appState.message = "Expense Deleted";
                });
        }

    }

    app.register.controller('expensesController', expensesController);
})
