define(['app/app', 'services', 'directives'], function(app) {
    expensesController.$inject = ['$location', '$scope', 'appState', 'getService', 'postService', 'deleteService', 'identityHandlerService', 'putService', '$anchorScroll'];
    function expensesController($location, $scope, appState, getService, postService, deleteService, identityHandlerService, putService, $anchorScroll) {

        appState.error = appState.message = null;
        $scope.editMode = false;
        $scope.newExpense = {date:new Date(), description:''};

        $scope.reload = function() {
            getService($scope, '/expense/', {organization:appState.current_organization.id}, 'expenses');
            //getService($scope, '/items/', {}, 'items');
            getService($scope, '/categories/', {}, 'categories');
            $scope.newExpense = {date:new Date(), description:''};
        };

        $scope.setEditMode = function(id) {
            $scope.editMode = true;
            var temp = angular.copy($scope.expenses.filter(function(e){ return e.id == id;})[0]);
            temp.date = new Date(temp.date);
            $scope.newExpense = temp;
            $scope.newExpense.category = temp.category.toString();
            if(window.innerWidth < 900){
                $location.hash('editheader');
                $anchorScroll();
            }
        }

        $scope.cancelEdit = function() {
            $scope.editMode = false;
            $scope.newExpense = {date:new Date(), description:''};
        }


        if(!appState.identity) {
            identityHandlerService().then(function(response) {
                $scope.reload();
            });
        }
        else {
            $scope.reload();
        }

        $scope.addExpense = function() {
            var t, msg;
            if($scope.editMode) {
                t = putService('/expense/'+$scope.newExpense.id+'/?organization='+appState.current_organization.id.toString(), $scope.newExpense);
                msg = "Expense Updated";
            }
            else {
                t = postService('/expense/', $scope.newExpense);
                msg = "Expense Added";
            }

            t.then(function(response) {
                    $scope.reload();
                    appState.message = msg;
                });
            $scope.cancelEdit();
        }

        $scope.remove = function(id) {
            deleteService('/expense/'+id, {organization:appState.current_organization.id})
                .then(function(response) {
                    $scope.reload();
                    appState.message = "Expense Deleted";
                });
        }

    }

    app.register.controller('expensesController', expensesController);
})
