package com.bee.platform.cloud.si.manufacture.service.factoryconfig;

import com.bee.platform.cloud.si.manufacture.dto.ConfigCodeDTO;
import com.bee.platform.cloud.si.manufacture.entity.ConfigCode;
import com.baomidou.mybatisplus.service.IService;

import java.util.List;

/**
 * <p>
 * 码表 服务类
 * </p>
 *
 * @author chenxm66777123
 * @since 2019-09-23
 */
public interface ConfigCodeService extends IService<ConfigCode> {

    List<ConfigCodeDTO> listConfigCode(String type);
}
