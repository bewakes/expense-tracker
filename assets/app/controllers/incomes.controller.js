define(['app/app', 'services', 'directives'], function(app) {
    incomesController.$inject = ['$location', '$scope', 'appState', 'getService', 'postService', 'deleteService', 'identityHandlerService', 'putService', '$anchorScroll'];
    function incomesController($location, $scope, appState, getService, postService, deleteService, identityHandlerService, putService, $anchorScroll) {

        appState.error = appState.message = null;
        $scope.editMode = false;
        $scope.newIncome = {date:new Date(), description:''};

        $scope.reload = function() {
            $scope.no_more = false;
            $scope.offset = 0;
            $scope.expenses_by_date = [];
            var data = {};
            data.organization=appState.current_organization.id;
            data.offset = $scope.offset;
            getService($scope, '/expense/', data, 'expenses')
                .then(function() {
                    $scope.expenses_by_date = $scope.expenses_by_date.concat($scope.expenses);
                });
            getService($scope, '/categories/', {}, 'categories');
            $scope.newIncome = {date:new Date(), description:''};

            data = angular.copy(data);
            delete data.offset;
            data.top=1;
            getService($scope, '/expense/',data, 'sorted_expenses');

            $scope.date_expense = {}; // store expenses by date key
        };

        $scope.setEditMode = function(expense) {
            $scope.editMode = true;
            var temp = angular.copy(expense);
            temp.date = new Date(temp.date);
            $scope.newIncome = temp;
            $scope.newIncome.category = temp.category.toString();
            if(window.innerWidth < 900){
                $location.hash('editheader');
                $anchorScroll();
            }
        }
        $scope.getIncomesForDate = function(date) {
            if ($scope.date_expense[date]==undefined){
                $scope.date_expense[date] = {};
                $scope.date_expense[date].expenses = [];
                getService($scope.date_expense[date], '/expense/',{organization:appState.current_organization.id,forDate:date}, 'expenses')
                    .then(function(d){
                        $scope.date_expense[date].show = true;
                    });
            }
            else {
                if ($scope.date_expense[date].show) $scope.date_expense[date].show=false;
                else $scope.date_expense[date].show=true;
            }
        }

        $scope.cancelEdit = function() {
            $scope.editMode = false;
            $scope.newIncome = {date:new Date(), description:''};
        }

        $scope.showIncomes = function(date) {
        }

        $scope.loadMore = function () {
            $scope.offset +=1;
            var data = {
                organization:appState.current_organization.id,
                offset:$scope.offset,
            }
            getService($scope, '/expense/', data, 'tempexpenses')
                .then(function(d) {
                    $scope.expenses_by_date = $scope.expenses_by_date.concat($scope.tempexpenses);
                    if($scope.tempexpenses.length==0)
                        $scope.no_more = true;
                });
        }


        if(!appState.identity) {
            identityHandlerService().then(function(response) {
                $scope.reload();
            });
        }
        else {
            $scope.reload();
        }

        $scope.addIncome = function() {
            var t, msg;
            if($scope.editMode) {
                t = putService('/expense/'+$scope.newIncome.id+'/?organization='+appState.current_organization.id.toString(), $scope.newIncome);
                msg = "Income Updated";
            }
            else {
                t = postService('/expense/', $scope.newIncome);
                msg = "Income Added";
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
                    appState.message = "Income Deleted";
                });
        }

    }

    app.register.controller('expensesController', expensesController);
})
