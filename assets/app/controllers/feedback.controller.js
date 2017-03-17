define(['app/app', 'services', 'directives'], function(app) {
    feedbackController.$inject = ['$scope', '$location', 'appState', 'postService', 'identityHandlerService'];
    function feedbackController($scope, $location, appState, postService, identityHandlerService) {
        appState.error = appState.message = null;

        $scope.reload = function() {
            $scope.feedback = {};
            $scope.feedback.user = appState.identity.id;
        };

        if(!appState.identity) {
            identityHandlerService().then(function(response){
                $scope.reload();
            });
        }
        else {
            $scope.reload();
        }


        $scope.addFeedback = function() {
            postService('/feedback/', $scope.feedback)
                .then(function(response) {
                    $scope.reload();
                    appState.message="Thank you so much!!";
                });
        }
    }

    app.register.controller('feedbackController', feedbackController);
})
