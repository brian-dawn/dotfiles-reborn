{ nixpkgs.config.allowUnfree = true; 
  allowUnfree = true;

  hardware.opengl = {
      driSupport = true;
      driSupport32Bit = true;
  };

  services.xserver.videoDrivers = [ "nvidia" ];
  
}

  
