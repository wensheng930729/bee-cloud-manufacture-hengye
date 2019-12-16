package com.bee.platform.cloud.si.manufacture.dao.mapper;

import com.baomidou.mybatisplus.mapper.BaseMapper;
import com.bee.platform.cloud.si.manufacture.entity.ProSample;
import com.bee.platform.cloud.si.manufacture.rq.ProBaggingFirstRq;

/**
 * <p>
 * 生产样品表 Mapper 接口
 * </p>
 *
 * @author liliang123
 * @since 2019-09-27
 */
public interface ProSampleMapper extends BaseMapper<ProSample> {

    /**
     * @Description 查询记录绑定装袋信息
     * @author chenxm66777123
     * @Date 2019/10/9 11:34
     * @version 1.0.0
     */
    ProSample getResultForBindBagging(ProBaggingFirstRq rq);

}
