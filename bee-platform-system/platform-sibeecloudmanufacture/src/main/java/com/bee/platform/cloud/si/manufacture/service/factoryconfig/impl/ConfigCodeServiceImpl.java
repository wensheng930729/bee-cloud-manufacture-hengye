package com.bee.platform.cloud.si.manufacture.service.factoryconfig.impl;

import com.baomidou.mybatisplus.mapper.EntityWrapper;
import com.bee.platform.cloud.si.manufacture.dto.ConfigCodeDTO;
import com.bee.platform.cloud.si.manufacture.entity.ConfigCode;
import com.bee.platform.cloud.si.manufacture.dao.mapper.ConfigCodeMapper;
import com.bee.platform.cloud.si.manufacture.service.factoryconfig.ConfigCodeService;
import com.baomidou.mybatisplus.service.impl.ServiceImpl;
import com.bee.platform.common.utils.BeanUtils;
import org.springframework.stereotype.Service;

import java.util.List;

/**
 * <p>
 * 码表 服务实现类
 * </p>
 *
 * @author chenxm66777123
 * @since 2019-09-23
 */
@Service
public class ConfigCodeServiceImpl extends ServiceImpl<ConfigCodeMapper, ConfigCode> implements ConfigCodeService {


    @Override
    public List<ConfigCodeDTO> listConfigCode(String type) {

        List<ConfigCode> codeList = selectList(new EntityWrapper<>(new ConfigCode().setType(type)));

        return BeanUtils.assemble(ConfigCodeDTO.class,codeList);
    }
}
