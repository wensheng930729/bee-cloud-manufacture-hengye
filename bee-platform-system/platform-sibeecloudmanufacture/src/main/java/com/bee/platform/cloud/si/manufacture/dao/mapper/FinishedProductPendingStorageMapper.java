package com.bee.platform.cloud.si.manufacture.dao.mapper;

import com.baomidou.mybatisplus.mapper.BaseMapper;
import com.baomidou.mybatisplus.plugins.pagination.Pagination;
import com.bee.platform.cloud.si.manufacture.dto.FinishedProductPendingStorageListDTO;
import com.bee.platform.cloud.si.manufacture.dto.ReportFormSaleDTO;
import com.bee.platform.cloud.si.manufacture.entity.FinishedProductPendingStorage;
import org.apache.ibatis.annotations.Param;

import java.math.BigDecimal;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * @ClassName: FinishedProductPendingStorageMapper
 * @Description: 待入库产成品Mapper
 * @Author: fei.sun
 * @Date: 2019/9/25 13:27
 * @Version: 1.0
 */
public interface FinishedProductPendingStorageMapper extends BaseMapper<FinishedProductPendingStorage> {
    /**
     * 查询已经入库且没有规格的产品数量
     * @param factoryId a
     * @param orgId b
     * @return 数量
     */
    BigDecimal selectNoSpecProduct(@Param("factoryId") Integer factoryId, @Param("orgId") Integer orgId);

    /**
     * web查询产成品
     * @param map
     * @param pagination 
     * @return
     */
	List<FinishedProductPendingStorageListDTO> selectFinishedStorageByConditional(HashMap<String, Object> map, Pagination pagination);
}
