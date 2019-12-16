package com.bee.platform.cloud.user.hystrix;

import com.bee.platform.cloud.user.service.feign.UserCloudManufactureBeatFeignClient;
import com.bee.platform.common.entity.ResCodeEnum;
import com.bee.platform.common.entity.ResponseResult;

import feign.hystrix.FallbackFactory;
import org.springframework.stereotype.Component;

@Component
public class UserCloudManufactureBeatFeignClientFallbackFactory implements FallbackFactory<UserCloudManufactureBeatFeignClient> {

	@Override
	public UserCloudManufactureBeatFeignClient create(Throwable cause) {

		return new UserCloudManufactureBeatFeignClient() {

			@Override
			public ResponseResult<String> beat() {
				return ResponseResult.buildResponseResult(ResCodeEnum.HYSTRIX_ENABLED, ResCodeEnum.HYSTRIX_ENABLED.msg);
			}

		};
	}

}
