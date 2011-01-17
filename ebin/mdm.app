{ application, mdm,                              % application's name
  [                                              % list application's attributes
    { description, "Meter Data Management" },    % . description
    { vsn, "0.2.0" },                            % . version number
    { modules,                                   % . List ALL the run-time modules.
      [ mdm_app,                                 % . . module with application behavior
        mdm_sup,                                 % . . module with supervisor behavior
        assets, usage, events, circuit           % . . modules with gen_server behavior
        ]                                        % . when list is long, maintain alphabetic order
      },
    { registered, [ assets, usage, events ] },  % . what process names will be registered?
    
    { applications, [ kernel, stdlib ] },        % . local applications to start prior to this one
    { mod, { mdm_app, [] }  }                    % . start-up module (with application behavior)
    ]
  }. 