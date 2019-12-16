package com.bee.platform.cloud.si.manufacture.dao.mapper;

import com.baomidou.mybatisplus.mapper.BaseMapper;
import com.bee.platform.cloud.si.manufacture.dto.FinishedProductFreeOutDetailDTO;
import com.bee.platform.cloud.si.manufacture.entity.FinishedProductFreeOut;
import org.apache.ibatis.annotations.Param;
import com.bee.platform.cloud.si.manufacture.entity.FinishedProductFreeOut;

/**
 * @ClassName: FinishedProductFreeOutMapper
 * @Description: java类作用描述
 * @Author: fei.sun
 * @Date: 2019/11/18 11:14
 * @Version: 1.0
 */
public interface FinishedProductFreeOutMapper extends BaseMapper<FinishedProductFreeOut> {
    /**
     * 根据出库车辆和仓库id查询
     * @param contractId
     * @param storageId
     * @return
     */
    FinishedProductFreeOutDetailDTO selectByCarId(@Param("contractId") String contractId, @Param("storageId") Integer storageId);
}
