{ application, mdm_application,                  % application's name
  [                                                   % list application's attributes
    { description, "Meter Data Management" },    % . description
    { vsn, "0.1.0" },                                 % . version number
    { modules,                                   % . List ALL the run-time modules.
      [ mdm_app,                                 % . . module with application behavior
        mdm_sup,                                 % . . module with supervisor behavior
        assets, usage, events, world_base        % . . modules with gen_server behavior
        ]                                        % . when list is long, maintain alphabetic order
      },
    { registered, [ mdm_app, mdm_sup, assets, usage, events ] },  % . what process names will be registered?
    
    { applications, [ kernel, stdlib, mnesia ] },        % . applications to start prior to this one
    { mod, { mdm_app, [] }  }                    % . start-up module (with application behavior)
    ]
  }. 