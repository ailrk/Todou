self: { config, lib, pkgs, ...}:
let
  cfgs = config.services.todou;
in
{
  options.services.todou =
  let submodule = { ... }:
    {
      options = {
        enable = lib.mkOption {
          type = lib.types.bool;
          default = false;
          description = ''
            Enable todou
          '';
        };

        storage = lib.mkOption {
          type = lib.types.string;
          description = ''
            List of local file system paths to serve.
            Todou supports three storage backends:
              - file system: dir:<dirname>
              - s3:          s3:<bucket>
              - sqlite:      sqlite:<connection string>
          '';
        };

        port = lib.mkOption {
          type = lib.types.int;
          default = 5000;
          description = ''
            Port to serve
          '';
        };

        environment = lib.mkOption {
          type = lib.types.nullOr (lib.types.either lib.types.str lib.types.path);
          default = null;
          description = ''
            Path to the environment variable file. This file will be used on the systemd service.

            S3 environment:
            Todou accepts the following AWS credentials:
              AWS_ACCESS_KEY_ID, AWS_SECRET_ACCESS_KEY, AWS_ENDPOINT_URL, AWS_DEFAULT_REGION.
          '';
        };

        package = lib.mkOption {
          type = lib.types.package;
          default = self.packages.${pkgs.system}.default;
        };
      };
    };
  in
    lib.mkOption {
      description = "Configure todou instances.";
      default = {};
      type = lib.types.attrsOf (lib.types.submodule submodule);
    };


  config = lib.mkIf (lib.any (cfg: cfg.enable) (lib.attrValues cfgs)) {
    systemd.services =
      let
        mkService = name: cfg:
        let
          serviceName = "todou-" + name;
          port = builtins.toString cfg.port;
        in
          lib.nameValuePair
            serviceName
            {
              wantedBy = [ "multi-user.target" ];
              after = [ "network.target" ];
              description = "Start ${serviceName}";
              path = [ pkgs.getent ];
              serviceConfig = {
                Type = "simple";
                EnvironmentFile = if cfg.environment == null then "" else cfg.environment;
                ExecStart = "${cfg.package}/bin/todou --port=${port} --storage=\"${cfg.storage}\"";
              };
            };
      in
      lib.mapAttrs' mkService cfgs ;
  };
}
