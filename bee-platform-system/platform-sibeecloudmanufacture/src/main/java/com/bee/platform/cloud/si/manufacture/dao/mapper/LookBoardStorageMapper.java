package com.bee.platform.cloud.si.manufacture.dao.mapper;

import com.bee.platform.cloud.si.manufacture.dto.OutStorageDTO;
import com.bee.platform.cloud.si.manufacture.dto.OutStorageListDTO;
import com.bee.platform.cloud.si.manufacture.dto.RawMaterialDTO;
import org.apache.ibatis.annotations.Param;

import java.math.BigDecimal;
import java.util.List;
import java.util.Map;

/**
 * @ClassName: LookBoardStorageMapper
 * @Description: java类作用描述
 * @Author: fei.sun
 * @Date: 2019/10/18 16:11
 * @Version: 1.0
 */
public interface LookBoardStorageMapper {
    /**
     * 根据产品类别查询产品的库存量
     * @param typeName 类别名称
     * @param factoryId 工厂id
     * @param orgId 企业id
     * @return aa
     */
    List<RawMaterialDTO> selectRawMaterialByType(@Param("typeName") String typeName,@Param("factoryId") Integer factoryId
            ,@Param("orgId") Integer orgId);

    /**
     * 根据时间查询原料出库情况
     * @param startTime 起始时间
     * @param endTime 结束时间
     * @return aa
     */
    List<OutStorageDTO> selectRawMaterialOutStorageByTime(@Param("startTime") String startTime, @Param("endTime") String endTime);

    /**
     * 查询产生品在这段时间的入库量
     * @param productId 产生品id
     * @param startDate 起始时间
     * @param endDate 结束时间
     * @return 产品入库量
     */
    BigDecimal selectFinishedProductInStorageByRangTime(@Param("productId") Integer productId, @Param("startTime") String startDate
            , @Param("endDate") String endDate);

    /**
     * 查询原料在这段时间的入库量
     * @param productId 产生品id
     * @param startDate 起始时间
     * @param endDate 结束时间
     * @return 产品入库量
     */
    BigDecimal selectRawMaterialInStorageByRangTime(@Param("productId") Integer productId, @Param("startTime") String startDate
            , @Param("endDate") String endDate);

    /**
     * 自由入库在这段时间的入库量
     * @param productId 产生品id
     * @param startDate 起始时间
     * @param endDate 结束时间
     * @return 产品入库量
     */
    BigDecimal selectFreeInStorageNumberByRangtime(@Param("productId") Integer productId, @Param("startTime") String startDate
            , @Param("endDate") String endDate);

    /**
     * 成品出库量
     * @param productId 产生品id
     * @param startDate 起始时间
     * @param endDate 结束时间
     * @return 产品出库量
     */
    BigDecimal selectFinishedProductOutStorageByRangTime(@Param("productId") Integer productId, @Param("startTime") String startDate
            , @Param("endDate") String endDate);

    /**
     *
     * 原料出库量
     * @param productId 产生品id
     * @param startDate 起始时间
     * @param endDate 结束时间
     * @return 原料出库量
     */
    BigDecimal selectRawMaterialsProductOutStorageByRangTime(@Param("productId") Integer productId, @Param("startTime") String startDate
            , @Param("endDate") String endDate);

    /**
     * 新增出库量
     * @param productId 产生品id
     * @param startDate 起始时间
     * @param endDate 结束时间
     * @return 新增出库量
     */
    BigDecimal selectFreeOutStorageNumberByRangtime(@Param("productId") Integer productId, @Param("startTime") String startDate
            , @Param("endDate") String endDate);

    /**
     * 查询成品出库数量
     * @param map 参数map
     * @return b
     */
    List<OutStorageListDTO> selectFinishedProductOutNumber(@Param("params") Map<String,Object> map);

    /**
     * 查询原料出库数量
     * @param params a
     * @return b
     */
    List<OutStorageListDTO> selectRawMaterialProductOutNumber(@Param("params") Map<String, Object> params);


    /**
     * 查询新增出库量根据时间和产品分组
     * @param params 参数集合
     * @return a
     */
    List<OutStorageListDTO> selectFreeOutStorageNumberRangTime(@Param("params") Map<String, Object> params);

    /**
     * 查询在途量
     * @param typeName 产品类别名称
     * @param orgId 企业id
     * @param factoryId 工厂id
     * @return a
     */
    List<RawMaterialDTO> selectInTransit(@Param("typeName") String typeName, @Param("orgId") Integer orgId
            , @Param("factoryId") Integer factoryId);
}
