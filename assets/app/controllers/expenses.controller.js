define(['app/app', 'services', 'directives'], function(app) {
    expensesController.$inject = ['$scope', 'appState', 'getService', 'postService', 'deleteService', 'identityHandlerService', 'putService'];
    function expensesController($scope, appState, getService, postService, deleteService, identityHandlerService, putService) {

        appState.error = appState.message = null;
        $scope.editMode = false;

        $scope.setEditMode = function(id) {
            $scope.editMode = true;
            var temp = angular.copy($scope.expenses.filter(function(e){ return e.id == id;})[0]);
            temp.date = new Date(temp.date);
            $scope.newExpense = temp;
            $scope.newExpense.item = temp.item.toString();
        }

        $scope.cancelEdit = function() {
            $scope.editMode = false;
            $scope.newExpense = {date:new Date(), description:''};
        }

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
            var t, msg;
            if($scope.editMode) {
                t = putService('/expense/'+$scope.newExpense.id+'/', $scope.newExpense);
                msg = "Expense Updated";
            }
            else {
                t = postService('/expense/', $scope.newExpense);
                msg = "Expense Added";
            }

            t.then(function(response) {
                    getService($scope, '/expense/', {organization:appState.current_organization.id}, 'expenses');
                    $scope.newExpense = {date:new Date(), description:''};
                    appState.message = msg;
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
