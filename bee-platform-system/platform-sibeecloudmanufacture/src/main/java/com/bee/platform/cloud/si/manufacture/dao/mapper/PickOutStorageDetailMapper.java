package com.bee.platform.cloud.si.manufacture.dao.mapper;

import com.bee.platform.cloud.si.manufacture.dto.PickOutStorageDetailDTO;
import com.bee.platform.cloud.si.manufacture.entity.PickOutStorageDetail;
import com.baomidou.mybatisplus.mapper.BaseMapper;
import org.apache.ibatis.annotations.Param;

import java.math.BigDecimal;
import java.util.List;
import java.util.Map;

/**
 * <p>
 * 领用出库明细表 Mapper 接口
 * </p>
 *
 * @author MP123
 * @since 2019-09-27
 */
public interface PickOutStorageDetailMapper extends BaseMapper<PickOutStorageDetail> {

    /**
     * @Description TODO
     * @author chenxm66777123
     * @Date 2019/11/6 15:33
     * @version 1.0.0
     */
    BigDecimal getTodayUsed(Map<String,Object> map);

    /**
     * @Description TODO
     * @author chenxm66777123
     * @Date 2019/11/6 15:37
     * @version 1.0.0
     */
    BigDecimal getTodayUsedAfter(Map<String,Object> map);

    List<PickOutStorageDetailDTO> getNewOutOfStockList(@Param("enterpriseId") Integer enterpriseId,@Param("factoryId") Integer factoryId);
}
