package com.bee.platform.cloud.si.manufacture.dao.mapper;

import com.baomidou.mybatisplus.mapper.BaseMapper;
import com.bee.platform.cloud.si.manufacture.entity.WeightMachineData;
import org.springframework.stereotype.Component;

/**
 * @Description 地磅称重数据 Mapper 接口
 * @author chenxm66777123
 * @Date 2019/9/26 9:17
 * @version 1.0.0
 */
@Component
public interface WeightMachineDataMapper extends BaseMapper<WeightMachineData> {


    WeightMachineData getReverseOneResult(String deviceId);
}
