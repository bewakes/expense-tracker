define(['app/app', 'services', 'directives'], function(app) {
    incomesController.$inject = ['$location', '$scope', 'appState', 'getService', 'postService', 'deleteService', 'identityHandlerService', 'putService', '$anchorScroll'];
    function incomesController($location, $scope, appState, getService, postService, deleteService, identityHandlerService, putService, $anchorScroll) {

        appState.error = appState.message = null;
        $scope.editMode = false;
        $scope.newIncome = {date:new Date(), description:''};

        $scope.reload = function() {
            $scope.no_more = false;
            $scope.offset = 0;
            $scope.incomes_by_date = [];
            var data = {};
            data.organization=appState.current_organization.id;
            data.offset = $scope.offset;
            getService($scope, '/income/', data, 'incomes')
                .then(function() {
                    $scope.incomes_by_date = $scope.incomes_by_date.concat($scope.incomes);
                });
            getService($scope, '/categories/', {}, 'categories');
            $scope.newIncome = {date:new Date(), description:''};

            data = angular.copy(data);
            delete data.offset;
            data.top=1;
            getService($scope, '/income/',data, 'sorted_incomes');

            $scope.date_income = {}; // store incomes by date key
        };

        $scope.setEditMode = function(income) {
            $scope.editMode = true;
            var temp = angular.copy(income);
            temp.date = new Date(temp.date);
            $scope.newIncome = temp;
            $scope.newIncome.category = temp.category.toString();
            if(window.innerWidth < 900){
                $location.hash('editheader');
                $anchorScroll();
            }
        }
        $scope.getIncomesForDate = function(date) {
            if ($scope.date_income[date]==undefined){
                $scope.date_income[date] = {};
                $scope.date_income[date].incomes = [];
                getService($scope.date_income[date], '/income/',{organization:appState.current_organization.id,forDate:date}, 'incomes')
                    .then(function(d){
                        $scope.date_income[date].show = true;
                    });
            }
            else {
                if ($scope.date_income[date].show) $scope.date_income[date].show=false;
                else $scope.date_income[date].show=true;
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
            getService($scope, '/income/', data, 'tempincomes')
                .then(function(d) {
                    $scope.incomes_by_date = $scope.incomes_by_date.concat($scope.tempincomes);
                    if($scope.tempincomes.length==0)
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
                t = putService('/income/'+$scope.newIncome.id+'/?organization='+appState.current_organization.id.toString(), $scope.newIncome);
                msg = "Income Updated";
            }
            else {
                t = postService('/income/', $scope.newIncome);
                msg = "Income Added";
            }

            t.then(function(response) {
                    $scope.reload();
                    appState.message = msg;
                });
            $scope.cancelEdit();
        }

        $scope.remove = function(id) {
            deleteService('/income/'+id, {organization:appState.current_organization.id})
                .then(function(response) {
                    $scope.reload();
                    appState.message = "Income Deleted";
                });
        }

    }

    app.register.controller('incomesController', incomesController);
})
