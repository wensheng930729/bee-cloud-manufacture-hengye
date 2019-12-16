package com.bee.platform.cloud.si.manufacture.dao.mapper;

import com.bee.platform.cloud.si.manufacture.dto.ProFurnacePowerConsumeDTO;
import com.bee.platform.cloud.si.manufacture.entity.ProOreFurnaceRecordDetail;
import com.baomidou.mybatisplus.mapper.BaseMapper;
import com.bee.platform.cloud.si.manufacture.rq.ProStatisticRQ;

import java.util.List;

/**
 * <p>
 * 矿热炉记录明细表 Mapper 接口
 * </p>
 *
 * @author huangxin123
 * @since 2019-09-26
 */
public interface ProOreFurnaceRecordDetailMapper extends BaseMapper<ProOreFurnaceRecordDetail> {

    /**
     * @descriptin 条件查询矿热炉电力消耗
     * @author xin.huang
     * @param rq
     * @date 2019/10/22
     * @return
     */
    List<ProFurnacePowerConsumeDTO> findPowerConsume(ProStatisticRQ rq);

    /**
     * @descriptin 条件查询各炉号的总电耗
     * @author xin.huang
     * @param rq
     * @date 2019/10/25
     * @return
     */
    List<ProFurnacePowerConsumeDTO> findTotalPowerConsume(ProStatisticRQ rq);

}
