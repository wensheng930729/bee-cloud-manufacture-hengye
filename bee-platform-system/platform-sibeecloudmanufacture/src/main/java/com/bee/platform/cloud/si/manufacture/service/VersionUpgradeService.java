package com.bee.platform.cloud.si.manufacture.service;

import com.baomidou.mybatisplus.service.IService;
import com.bee.platform.cloud.si.manufacture.dto.VersionUpgradeDTO;
import com.bee.platform.cloud.si.manufacture.entity.VersionUpgrade;
import com.bee.platform.common.entity.ResponseResult;

import java.util.List;

/**
 * <p>
 * 版本升级描述 服务类
 * </p>
 *
 * @author LL123
 * @since 2019-11-25
 */
public interface VersionUpgradeService extends IService<VersionUpgrade> {

    /**
     * 根据版本号查询升级描述
     */
    public ResponseResult<List<VersionUpgradeDTO>> getByVersionNum(String versionNum);

}
