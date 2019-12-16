package com.bee.platform.cloud.user.service.impl;

import com.baomidou.mybatisplus.service.impl.ServiceImpl;
import com.bee.platform.cloud.user.dao.mapper.ArUserMapper;
import com.bee.platform.cloud.user.entity.ArUser;
import com.bee.platform.cloud.user.service.ArUserService;

import org.springframework.stereotype.Service;

/**
 * @author zhigang.zhou
 * @since 2019-09-16
 */
@Service
public class ArUserServiceImpl extends ServiceImpl<ArUserMapper, ArUser> implements ArUserService {

}
