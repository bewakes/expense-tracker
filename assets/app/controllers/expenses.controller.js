define(['app/app', 'services', 'directives'], function(app) {
    expensesController.$inject = ['$location', '$scope', 'appState', 'getService', 'postService', 'deleteService', 'identityHandlerService', 'putService', '$anchorScroll'];
    function expensesController($location, $scope, appState, getService, postService, deleteService, identityHandlerService, putService, $anchorScroll) {

        appState.error = appState.message = null;

        function formatExpenseDate(expense) {
            expense.date = expense.date.toJSON().substr(0, 10);
            return expense
        }

        $scope.editMode = false;
        $scope.newExpense = {date:new Date(), description:''};
        $scope.durations = [
            {name:'Last Week', value:'week', n:1},
            {name:'2 Weeks', value:'week', n:2},
            {name:'Last Month', value:'month', n:1},
            {name:'2 Months', value:'month', n:2},
            {name:'3 Months', value:'month', n:3},
            {name:'6 Months', value:'month', n:6},
            {name:'Last Year', value:'year', n:1}
        ];
        $scope.searchResults = [];

        $scope.search = {
            querystring: null,
            duration: null,
        }

        $scope.reload = function() {
            $scope.no_more = false;
            $scope.offset = 0;
            $scope.expenses_by_date = [];
            var data = {};
            data.organization=appState.current_organization.id;
            data.offset = $scope.offset;
            getService($scope, '/expense/', data, 'expenses')
                .then(function() {
                    $scope.expenses = $scope.expenses.map(function(x){x.show=false;return x;});
                    $scope.expenses_by_date = $scope.expenses_by_date.concat($scope.expenses);
                });
            getService($scope, '/categories/', {}, 'categories');
            $scope.newExpense = {date:new Date(), description:''};

            $scope.date_expense = {}; // store expenses by date key
        };

        $scope.setEditMode = function(expense) {
            $scope.editMode = true;
            var temp = angular.copy(expense);
            temp.date = new Date(temp.date);
            $scope.newExpense = temp;
            $scope.newExpense.category = temp.category.toString();
            if(window.innerWidth < 900){
                $location.hash('editheader');
                $anchorScroll();
            }
        }
        $scope.getExpensesForDate = function(date, item) {
            if ($scope.date_expense[date]==undefined){
                $scope.date_expense[date] = {};
                $scope.date_expense[date].expenses = [];
                getService($scope.date_expense[date], '/expense/',{organization:appState.current_organization.id,forDate:date}, 'expenses')
                    .then(function(d){
                        //$scope.date_expense[date].show = true;
                        item.show = !item.show;
                    });
            }
            else {
                item.show = !item.show;
            }
        }

        $scope.cancelEdit = function() {
            $scope.editMode = false;
            $scope.newExpense = {date:new Date(), description:''};
        }

        $scope.showExpenses = function(date) {
        }

        $scope.loadMore = function () {
            $scope.offset +=1;
            var data = {
                organization:appState.current_organization.id,
                offset:$scope.offset,
            }
            getService($scope, '/expense/', data, 'tempexpenses')
                .then(function(d) {
                    $scope.tempexpenses = $scope.tempexpenses.map(function(x){ x.show=false;return x;});
                    $scope.expenses_by_date = $scope.expenses_by_date.concat($scope.tempexpenses);
                    if($scope.tempexpenses.length==0)
                        $scope.no_more = true;
                });
        }


        if(!appState.identity) {
            identityHandlerService().then(function(response) {
                console.log('reloading after identity');
                $scope.reload();
            });
        }
        else {
                console.log('just reloading');
            $scope.reload();
        }

        $scope.addExpense = function() {
            var t, msg;
            if($scope.editMode) {
                t = putService(
                    '/expense/'+$scope.newExpense.id+'/?organization='+appState.current_organization.id.toString(),
                    formatExpenseDate($scope.newExpense)
                );
                msg = "Expense Updated";
            }
            else {
                t = postService('/expense/', formatExpenseDate($scope.newExpense));
                msg = "Expense Added";
            }

            t.then(function(response) {
                    $scope.reload();
                    appState.message = msg;
                });
            $scope.cancelEdit();
        }

        $scope.remove = function(id) {
            deleteService('/expense/'+id+'/', {organization:appState.current_organization.id})
                .then(function(response) {
                    $scope.reload();
                    appState.message = "Expense Deleted";
                });
        }

        $scope.sendQuery = function() {
            $scope.searchResults = [];
            // perpare data to send
            var data = {};
            data.organization=appState.current_organization.id;
            if ($scope.search.duration == null && ($scope.search.querystring == null || $scope.search.querystring.trim() == '')) {
                return;
            }
            if ($scope.search.duration != null) {
                data.duration = $scope.search.duration.value;
                data.n = $scope.search.duration.n;
            }
            if ($scope.search.querystring != null && $scope.search.querystring.trim() != '') {
                data.query = $scope.search.querystring;
            }
            getService($scope, '/expense/', data, 'searchResults')
                .then(function() {
                    $scope.searchResults = $scope.searchResults.map(function(x) {
                        x.show = false;
                        return x;
                    });
                    //$scope.expenses_by_date = $scope.expenses_by_date.concat($scope.expenses);
                });
        }

        $scope.changeDuration = function() {
            console.log(".."+$scope.search.durationindex);
            var ind = $scope.search.durationindex;
            if(ind != null && ind.trim() != '') {
                $scope.search.duration = $scope.durations[$scope.search.durationindex];
            }
            console.log($scope.search.duration);
            $scope.sendQuery();
        }
        $scope.changeQueryString = function() {
        }
    }
    app.register.controller('expensesController', expensesController);
})
