package com.bee.platform.cloud.si.manufacture.dao.mapper;

import com.baomidou.mybatisplus.mapper.BaseMapper;
import com.baomidou.mybatisplus.plugins.pagination.Pagination;
import com.bee.platform.cloud.si.manufacture.dto.ReportFormTestQualityTestDTO;
import com.bee.platform.cloud.si.manufacture.dto.SampleAssayAbandonDTO;
import com.bee.platform.cloud.si.manufacture.dto.SampleAssayAlreadyDTO;
import com.bee.platform.cloud.si.manufacture.dto.SampleAssayPrepareAndAssayingDTO;
import com.bee.platform.cloud.si.manufacture.entity.BuySample;
import org.apache.ibatis.annotations.Param;

import java.util.List;
import java.util.Map;

/**
 * <p>
 * 采购取样表 Mapper 接口
 * </p>
 *
 * @author liliang123
 * @since 2019-09-23
 */
public interface BuySampleMapper extends BaseMapper<BuySample> {

    /**
     * 查询化验中和待化验列表
     */
    List<SampleAssayPrepareAndAssayingDTO> getAssayingAndPrepareList(Pagination pagination, Map map);
    /**
     * 查询已弃样列表
     */
    List<SampleAssayAbandonDTO> getAssayAbandonList(Pagination pagination, Map<String, Object> map);
    /**
     * 查询已化验列表
     */
    List<SampleAssayAlreadyDTO> getAssayAlreadyList(Pagination pagination, Map<String, Object> map);
    /**
     * 查询磅单号相关的样品
     * @param machineId
     * @return
     */
    List<BuySample> selectSampleByMachineId(@Param("machineId") String machineId);

    /**
     * 进出厂取样信息
     * @param pagination
     * @param map
     * @return
     */
    List<ReportFormTestQualityTestDTO> getSampleInfoINOutFactory(Pagination pagination, Map<String, Object> map);
}
