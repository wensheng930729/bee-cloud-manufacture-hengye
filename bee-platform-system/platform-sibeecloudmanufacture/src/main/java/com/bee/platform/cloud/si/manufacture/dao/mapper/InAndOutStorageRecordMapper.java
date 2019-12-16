package com.bee.platform.cloud.si.manufacture.dao.mapper;

import com.baomidou.mybatisplus.mapper.BaseMapper;
import com.baomidou.mybatisplus.plugins.pagination.Pagination;
import com.bee.platform.cloud.si.manufacture.dto.*;
import com.bee.platform.cloud.si.manufacture.entity.ArUser;
import org.apache.ibatis.annotations.Param;

import java.util.List;

/**
 * <p>
 *  Mapper 接口
 * </p>
 *
 * @author fei.sun
 * @since 2019-09-16
 */
public interface InAndOutStorageRecordMapper {

    /**
     * 查询采购入库记录
     * @param factoryId
     * @param orgId
     * @return
     */
    List<BuyInStorageRecordDTO> selectBuyInStorageRecord(@Param("factoryId") Integer factoryId, @Param("orgId") Integer orgId);

    /**
     * 查询新增入库记录
     * @param factoryId
     * @param orgId
     * @return
     */
    List<FreeInStorageRecordDTO> selectFreeInStorageRecord(@Param("factoryId") Integer factoryId, @Param("orgId") Integer orgId);

    /**
     * 查询产成品入库记录
     * @param factoryId
     * @param orgId
     * @return
     */
    List<FinishProductInStorageRecordDTO> selectFinishProductInStorageRecord(@Param("factoryId") Integer factoryId, @Param("orgId") Integer orgId);

    /**
     * 查询销售出库记录
     * @param factoryId
     * @param orgId
     * @return
     */
    List<SaleOutStorageRecordDTO> selectSaleOutStorageRecord(@Param("factoryId") Integer factoryId, @Param("orgId") Integer orgId);

    /**
     * 查询领用出库记录
     * @param factoryId
     * @param orgId
     * @return
     */
    List<PickOutStorageDTO> selectPickOutStorage(@Param("factoryId") Integer factoryId, @Param("orgId") Integer orgId);

    /**
     * 查询生产出库信息
     * @return
     */
    List<ProductionOutStorageDTO> selectProductionOutStorage(@Param("factoryId") Integer factoryId, @Param("orgId") Integer orgId);

    /**
     *  销售新增出库记录
     * @param factoryId
     * @param orgId
     * @return
     */
    List<SaleOutStorageRecordDTO> selectSaleFreeOutStorageRecord(@Param("factoryId") Integer factoryId, @Param("orgId") Integer orgId);

	List<FreeInStorageRecordDTO> selectFreeInStorageRecordWithPage(@Param("factoryId") Integer factoryId, @Param("orgId") Integer orgId,
			Pagination pagination);
}
