package com.bee.platform.cloud.si.manufacture.dao.mapper;

import com.baomidou.mybatisplus.mapper.BaseMapper;
import com.bee.platform.cloud.si.manufacture.dto.ProFurnaceProductionDetailDTO;
import com.bee.platform.cloud.si.manufacture.dto.ReportFormConsumptionAnalysisAmountDTO;
import com.bee.platform.cloud.si.manufacture.entity.ProBlankingDetail;
import com.bee.platform.cloud.si.manufacture.rq.ProStatisticRQ;

import java.util.List;
import java.util.Map;

/**
 * <p>
 * 下料明细表 Mapper 接口
 * </p>
 *
 * @author huangxin123
 * @since 2019-10-21
 */
public interface ProBlankingDetailMapper extends BaseMapper<ProBlankingDetail> {

    /**
     * @descriptin 条件查询原料消耗
     * @author xin.huang
     * @param rq
     * @date 2019/10/21
     * @return
     */
    List<ProFurnaceProductionDetailDTO> findMaterialConsume(ProStatisticRQ rq);

    /**
     * @descriptin 条件查询主料或辅料消耗
     * @author xin.huang
     * @param rq
     * @date 2019/10/21
     * @return
     */
    List<ProFurnaceProductionDetailDTO> findMaterialMainOrAuxiliary(ProStatisticRQ rq);

    /**
     * 分班次查询产成品各原料消耗
     * @param params
     * @return
     */
    List<ReportFormConsumptionAnalysisAmountDTO> getConsumptionAnalysisAmount(Map<String, Object> params);

    /**
     * @descriptin 查询当日原料消耗数据
     * @author xin.huang
     * @param params
     * @date 2019/11/12
     * @return
     */
    List<ProFurnaceProductionDetailDTO> findCurrentMaterialConsume(Map<String, Object> params);

}
