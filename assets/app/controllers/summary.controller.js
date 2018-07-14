define(['app/app', 'services', 'directives'], function(app) {
    summaryController.$inject = ['$scope', '$location', 'appState', 'getService', 'postService', 'deleteService', 'identityHandlerService'];
    function summaryController($scope, $location, appState, getService, postService, deleteService, identityHandlerService) {
        appState.error = appState.message = null;
        $scope.durations = [
            {name:'Last Week', value:'week', n:1},
            {name:'2 Weeks', value:'week', n:2},
            {name:'Last Month', value:'month', n:1},
            {name:'2 Months', value:'month', n:2},
            {name:'3 Months', value:'month', n:3},
            {name:'6 Months', value:'month', n:6},
            {name:'Last Year', value:'year', n:1}
        ];

        $scope.dur_index = 0;

        $scope.expenses = [];
        $scope.reload = function() {
            $scope.summary_params = {
                organization: appState.current_organization.id,
                individual: true,
                duration: $scope.durations[$scope.dur_index].value,
                n: $scope.durations[$scope.dur_index].n,
            };
            $scope.getSummary();
        };

        if(!appState.identity) {
            identityHandlerService().then(function(response){
                $scope.reload();
            });
        }
        else {
            $scope.reload();
        }

        $scope.getSummary = function() {
            getService($scope, '/expense/', $scope.summary_params, 'expenses')
            .then(function(r) {
                $scope.total = $scope.expenses.reduce(function(a, x) { return a + x.cost;}, 0);
            });
        }
        
        $scope.changeDuration = function() {
            $scope.summary_params.duration = $scope.durations[$scope.dur_index].value;
            $scope.summary_params.n = $scope.durations[$scope.dur_index].n;
            console.log($scope.summary_params);
            $scope.getSummary();
        }
        
    }

    app.register.controller('summaryController', summaryController);
})
