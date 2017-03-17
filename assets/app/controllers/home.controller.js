define(['app/app', 'services','directives'], function(app) {

    homeController.$inject = ['$scope', '$location', 'appState', 'lineChartService', 'identityHandlerService'];
    function homeController($scope, $location, appState, lineChartService, identityHandlerService) {
        var date = new Date();
        function formatDate(date) {
            return date.getFullYear()+"-"+(date.getMonth()+1)+"-"+(date.getDate()+1);
        }
        var weekago = new Date();
        weekago.setDate(weekago.getDate()-7);

        $scope.durations = {
            'Last Week':{
                fromDate:formatDate(weekago),
                toDate:formatDate(date)
            },
            'Last Month': {
                fromDate:'2017-03-01',
                toDate:'2017-03-18'
            }
        };
        $scope.reload = function() {
            $scope.current_duration = 'Last Week';

            var data = angular.copy($scope.durations[$scope.current_duration]);
            data.organization=appState.current_organization.id;

            lineChartService($scope, '/expense/', data, $scope.current_duration);
        };

        if(!appState.identity) {
            identityHandlerService().then(function(response){
                $scope.reload();
            });
        }
        else {
            $scope.reload();
        }
    }
    app.register.controller('homeController', homeController);
})
