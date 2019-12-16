package com.bee.platform.cloud.si.manufacture.dao.mapper;

import com.baomidou.mybatisplus.mapper.BaseMapper;
import com.baomidou.mybatisplus.plugins.pagination.Pagination;
import com.bee.platform.cloud.si.manufacture.dto.ContractCarDTO;
import com.bee.platform.cloud.si.manufacture.dto.InAndOutStorageRecordDTO;
import com.bee.platform.cloud.si.manufacture.dto.SaleOutOfStockDTO;
import com.bee.platform.cloud.si.manufacture.dto.StorageIdAndNameDTO;
import com.bee.platform.cloud.si.manufacture.entity.FinishedProductBeOutOfStorage;
import com.bee.platform.cloud.si.manufacture.rq.SaleOutOfStockSearchRQ;
import org.apache.ibatis.annotations.Param;

import java.util.List;

/**
 * @ClassName: FinishedProductBeOutOfStorageMapper
 * @Description: 产生品待出库的车辆信息
 * @Author: fei.sun
 * @Date: 2019/9/26 16:52
 * @Version: 1.0
 */
public interface FinishedProductBeOutOfStorageMapper extends BaseMapper<FinishedProductBeOutOfStorage> {
    /**
     * 查询销售（产成品）出库记录
     * @param storageId c
     * @param productId c
     * @param productSpecId c
     * @return c
     */
    List<InAndOutStorageRecordDTO> selectSaleOutStorageRecord(@Param("storageId") Integer storageId, @Param("productId") Integer productId
            ,@Param("productSpecId") Integer productSpecId);

    /**
     * 根据合同号和仓库id查询出库车辆信息
     * @param contractId
     * @param storageId
     * @return
     */
    List<FinishedProductBeOutOfStorage> selectByContractAndStorage(@Param("contractId") String contractId, @Param("storageId") Integer storageId);

    /**
     * 根据吨袋编码获取仓库id和name
     *
     * @param tonCode
     * @return
     */
    StorageIdAndNameDTO getStorageIdAndNameByTonCode(@Param("tonCode") String tonCode);

    /**
     * 查询销售出库 新增出库数据
     * @param contractId
     * @param storageId
     * @return
     */
    List<FinishedProductBeOutOfStorage> selectFreeOutByContractAndStorage(@Param("contractId") String contractId, @Param("storageId") Integer storageId);



    List<ContractCarDTO> selectUnLoadContractCar(SaleOutOfStockSearchRQ rq, Pagination pagination);


    List<SaleOutOfStockDTO> searchSaleOutOfStockList(SaleOutOfStockSearchRQ rq, Pagination pagination);
}
