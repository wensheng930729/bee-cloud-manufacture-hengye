package com.bee.platform.cloud.si.manufacture.dao.mapper;

import com.baomidou.mybatisplus.mapper.BaseMapper;
import com.baomidou.mybatisplus.plugins.pagination.Pagination;
import com.bee.platform.cloud.si.manufacture.dto.*;
import com.bee.platform.cloud.si.manufacture.entity.ProBagging;
import com.bee.platform.cloud.si.manufacture.rq.ProStatisticRQ;

import java.math.BigDecimal;
import java.util.List;
import java.util.Map;

/**
 * @Description 成品装袋 Mapper 接口
 * @author chenxm66777123
 * @Date 2019/10/9 9:53
 * @version 1.0.0
 */
public interface ProBaggingMapper extends BaseMapper<ProBagging> {

    /**
     * @Description 获取装袋记录列表
     * @author chenxm66777123
     * @Date 2019/10/9 14:50
     * @version 1.0.0
     */
    List<ProBaggingDTO> getBaggingResults(Integer enterpriseId, Pagination pagination);

    /**
     * @descriptin 条件查询矿热炉产量
     * @author xin.huang
     * @param rq
     * @date 2019/10/21
     * @return
     */
    List<ProFurnaceProductionDetailDTO> findProduction(ProStatisticRQ rq);

    /**
     * @descriptin 条件查询矿热炉产出质量
     * @author xin.huang
     * @param rq
     * @date 2019/10/21
     * @return
     */
    List<ProFurnaceProductionDetailDTO> findProductSpecs(ProStatisticRQ rq);

    /**
     * 根据月份查询各个矿热炉的产品数量
     * @param param
     * @return
     */
    List<ReportFormPassRateAmountDTO> getPassRateAmount(Map<String, Object> param);

    /**
     * @descriptin 条件查询矿热炉总产量
     * @author xin.huang
     * @param rq
     * @date 2019/10/22
     * @return
     */
    ProFurnaceProductionDetailDTO findTotalProduction(ProStatisticRQ rq);

    /**
     * 根据炉次分页查询产量信息
     * @param param
     * @param pagination
     * @return
     */
    List<ReportFormYieldAnalysisDTO> getFurnaceBatchAmount(Map<String, Object> param, Pagination pagination);

    /**
     * 按规格 班次分组查询产量数据
     * @param map
     * @return
     */
    List<ReportFormAmountByShift> getOutputAmountBySpecsAndShift(Map<String, Object> map);

    /**
     * @descriptin 条件查询各矿热炉的总产量
     * @author xin.huang
     * @param rq
     * @date 2019/10/21
     * @return
     */
    List<ProFurnaceProductionDetailDTO> findFurnaceProduction(ProStatisticRQ rq);

    /**
     * @descriptin 查询当前时间下的产量
     * @author xin.huang
     * @param rq
     * @date 2019/10/28
     * @return
     */
    BigDecimal findCurrentProduction(ProStatisticRQ rq);
    /**
     * 查询金额总览
     *
     * @param map
     * @return
     */
    public BigDecimal getDataScreen(Map<String, Object> map);

    /**
     * 查询时间段内生产的产成品数量
     * @param params
     * @return
     */
    BigDecimal getCurrentTimeProductionAmount(Map<String, Object> params);

    /**
     * @descriptin 查询当日各成品产量
     * @author xin.huang
     * @param params
     * @date 2019/11/12
     * @return
     */
    List<BigScreenProductionDTO> findFurnaceFinishProduction(Map<String, Object> params);

}
