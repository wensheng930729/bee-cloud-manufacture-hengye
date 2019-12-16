package com.bee.platform.cloud.si.manufacture.service.manufactureproduce;

import com.baomidou.mybatisplus.plugins.pagination.Pagination;
import com.baomidou.mybatisplus.service.IService;
import com.bee.platform.cloud.si.manufacture.dto.ProArtificialFeedDTO;
import com.bee.platform.cloud.si.manufacture.entity.ProArtificialFeed;
import com.bee.platform.cloud.si.manufacture.rq.ProArtificialFeedRQ;
import com.bee.platform.common.entity.AuthPlatformUserInfo;
import com.bee.platform.common.entity.ResponseResult;

import java.util.List;

/**
 * <p>
 * 人工补料表 服务类
 * </p>
 *
 * @author huangxin123
 * @since 2019-09-25
 */
public interface ProArtificialFeedService extends IService<ProArtificialFeed> {

    ResponseResult artificialFeed(ProArtificialFeedRQ rq, AuthPlatformUserInfo userInfo);

    ResponseResult<List<ProArtificialFeedDTO>> findArtificialFeedList(AuthPlatformUserInfo userInfo, Pagination pagination);

}
