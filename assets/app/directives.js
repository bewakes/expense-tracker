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

    orgSelection.$inject = ['appState', 'identityHandlerService'];
    function orgSelection(appState, idhandler) {
        return {
            restrict: 'E',
            template: '<div style="float:right"><small>For: </small>\
                    <select class="fancy-select" ng-change="selectOrg()" ng-model="currentOrg">\
                    <option ng-repeat="org in orgs" value="{{org.id}}">{{org.name}}</li>\
                    </select>\
                    </div>',
            //transclude:true,
            scope:false,
            link: function(scope) {
                idhandler().then(function(){
                    /*if(!appState.current_organization) {*/
                        //alert('no current org')
                        //scope.currentOrg = appState.identity.default_organization.id.toString();
                    /*}*/
                    scope.currentOrg = appState.current_organization.id.toString();

                    scope.orgs = appState.identity.organizations;

                });

                scope.selectOrg = function() {
                    appState.current_organization = appState.identity
                        .organizations.filter(function(e) {
                            return e.id.toString()==scope.currentOrg.toString()
                        })[0];
                    scope.reload();
                };
            }
        }
    }

    app.register.directive('notifyMessage', notifyMessage);
    app.register.directive('orgSelection', orgSelection);
});
