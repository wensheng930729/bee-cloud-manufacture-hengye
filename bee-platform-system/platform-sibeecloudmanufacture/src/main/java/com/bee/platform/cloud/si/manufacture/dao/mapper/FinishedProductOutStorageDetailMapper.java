package com.bee.platform.cloud.si.manufacture.dao.mapper;

import com.baomidou.mybatisplus.plugins.pagination.Pagination;
import com.bee.platform.cloud.si.manufacture.dto.ReportFormProductWarehouseDTO;
import com.bee.platform.cloud.si.manufacture.dto.ReportProductOutStorageDTO;
import com.bee.platform.cloud.si.manufacture.dto.SaleOutStorageTonBagDTO;
import com.bee.platform.cloud.si.manufacture.dto.SampleQualityTestDTO;
import com.bee.platform.cloud.si.manufacture.entity.FinishedProductOutStorageDetail;
import com.baomidou.mybatisplus.mapper.BaseMapper;
import org.apache.ibatis.annotations.Param;

import java.util.List;
import java.util.Map;

/**
 * <p>
 * 按吨袋出库明细表 Mapper 接口
 * </p>
 *
 * @author chenxm66777123
 * @since 2019-09-26
 */
public interface FinishedProductOutStorageDetailMapper extends BaseMapper<FinishedProductOutStorageDetail> {

    /**
     * 批量插入吨袋出库明细表
     * @param list
     */
    void bulkInsert(List<FinishedProductOutStorageDetail> list);

    /**
     * 查询出库详情的吨袋信息
     * @param contractCarId
     * @return
     */
    List<SaleOutStorageTonBagDTO> selectSaleOutStorageTonBagDTOById(@Param("contractCarId") String contractCarId);

    /**
     * 查询质检编号及化验时间
     * @param contractCarId
     * @return
     */
    List<SampleQualityTestDTO> selectSampleInfo(@Param("contractCarId") String contractCarId);

    /**
     * 产成品入库报表
     * @param map
     * @param pagination
     * @return
     */
    List<ReportFormProductWarehouseDTO> getProductWarehouseReportFormByPage(Map<String, Object> map, Pagination pagination);


    /**
     * @Description 成品出库
     * @author chenxm66777123
     * @Date 2019/11/6 16:21
     * @version 1.0.0
     */
    List<ReportProductOutStorageDTO> getReportProductOutStorage(Map<String, Object> map, Pagination pagination);

    List<SaleOutStorageTonBagDTO> selectTonOutList(String contractCarId);
}
