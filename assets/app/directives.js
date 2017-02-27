define(['app/app'], function(app) {

    notifyMessage.$inject = ['appState'];
    function notifyMessage(appState) {
        return {
            restrict: 'E',
            template: '<div class="text-danger notification-msg" ng-show="error()" ng-bind="error()"></div>'+
                '<div class="text-success notification-msg" ng-show="message()" ng-bind="message()"></div>',
            transclude:true,
            scope: {},
            link: function(scope) {
                scope.error = function() {return appState.error;}
                scope.message = function() { return appState.message;}
            }

        }
    }

    app.register.directive('notifyMessage', notifyMessage);
});
