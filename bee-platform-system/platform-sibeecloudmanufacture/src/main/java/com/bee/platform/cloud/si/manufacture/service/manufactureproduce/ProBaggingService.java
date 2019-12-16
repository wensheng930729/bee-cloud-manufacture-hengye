package com.bee.platform.cloud.si.manufacture.service.manufactureproduce;

import com.baomidou.mybatisplus.plugins.pagination.Pagination;
import com.baomidou.mybatisplus.service.IService;
import com.bee.platform.cloud.si.manufacture.dto.ProBaggingDTO;
import com.bee.platform.cloud.si.manufacture.dto.ProBaggingDeatilsDTO;
import com.bee.platform.cloud.si.manufacture.dto.ProBaggingFirstDTO;
import com.bee.platform.cloud.si.manufacture.entity.ProBagging;
import com.bee.platform.cloud.si.manufacture.rq.ProBaggingFirstRq;
import com.bee.platform.cloud.si.manufacture.rq.ProBaggingSecondRq;
import com.bee.platform.common.entity.AuthPlatformUserInfo;
import com.bee.platform.common.entity.ResponseResult;

import java.util.List;

/**
 * @Description 成品装袋 服务类
 * @author chenxm66777123
 * @Date 2019/10/9 9:54
 * @version 1.0.0
 */
public interface ProBaggingService extends IService<ProBagging> {

    /**
     * @Description 保存成品装袋（第一步）
     * @author chenxm66777123
     * @Date 2019/10/9 10:05
     * @version 1.0.0
     */
    ResponseResult<ProBaggingFirstDTO> saveBaggingStepOne(ProBaggingFirstRq rq, AuthPlatformUserInfo userInfo);

    /**
     * @Description 保存成品装袋（第二步）
     * @author chenxm66777123
     * @Date 2019/10/9 10:05
     * @version 1.0.0
     */
    ResponseResult<String> saveBaggingStepTwo(ProBaggingSecondRq rq, AuthPlatformUserInfo userInfo);

    /**
     * @Description 获取装袋记录列表
     * @author chenxm66777123
     * @Date 2019/10/9 14:17
     * @version 1.0.0
     */
    List<ProBaggingDTO> getBaggingResults(AuthPlatformUserInfo userInfo, Pagination pagination);

    /**
     * @Description 获取装袋记录详细信息
     * @author chenxm66777123
     * @Date 2019/10/9 15:13
     * @version 1.0.0
     */
    ProBaggingDeatilsDTO getBaggingDeatils(ProBaggingDTO rq,AuthPlatformUserInfo userInfo);
}
