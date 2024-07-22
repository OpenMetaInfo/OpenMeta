package info.openmeta.starter.metadata.service.impl;

import info.openmeta.framework.orm.meta.OptionManager;
import info.openmeta.framework.orm.service.impl.EntityServiceImpl;
import info.openmeta.starter.metadata.entity.SysOptionSet;
import info.openmeta.starter.metadata.service.SysOptionSetService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.Lazy;
import org.springframework.stereotype.Service;

/**
 * SysOptionSet Model Service Implementation
 */
@Service
public class SysOptionSetServiceImpl extends EntityServiceImpl<SysOptionSet, Long> implements SysOptionSetService {

    @Autowired
    @Lazy
    private OptionManager optionManager;

    /**
     * Reload the memory cache of option set metadata.
     */
    public void reloadOptionManager() {
        optionManager.init();
    }
}