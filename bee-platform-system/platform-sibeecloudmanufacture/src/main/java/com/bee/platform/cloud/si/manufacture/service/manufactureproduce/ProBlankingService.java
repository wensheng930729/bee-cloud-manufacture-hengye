package com.bee.platform.cloud.si.manufacture.service.manufactureproduce;

import com.baomidou.mybatisplus.plugins.pagination.Pagination;
import com.baomidou.mybatisplus.service.IService;
import com.bee.platform.cloud.si.manufacture.dto.ProBlankingQueryDTO;
import com.bee.platform.cloud.si.manufacture.entity.ProBlanking;
import com.bee.platform.cloud.si.manufacture.rq.ProBlankingRQ;
import com.bee.platform.common.entity.AuthPlatformUserInfo;
import com.bee.platform.common.entity.ResponseResult;

import java.util.List;

/**
 * <p>
 * 下料表 服务类
 * </p>
 *
 * @author huangxin123
 * @since 2019-09-25
 */
public interface ProBlankingService extends IService<ProBlanking> {

    ResponseResult blanking(ProBlankingRQ rq, AuthPlatformUserInfo userInfo);

    ResponseResult<List<ProBlankingQueryDTO>> findList(ProBlankingRQ rq, AuthPlatformUserInfo userInfo, Pagination pagination);

}
