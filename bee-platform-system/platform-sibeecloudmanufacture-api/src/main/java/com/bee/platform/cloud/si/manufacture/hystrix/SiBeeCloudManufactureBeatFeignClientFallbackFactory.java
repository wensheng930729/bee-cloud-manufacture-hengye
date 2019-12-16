package com.bee.platform.cloud.si.manufacture.hystrix;

import com.bee.platform.cloud.si.manufacture.service.feign.SiBeeCloudManufactureBeatFeignClient;
import com.bee.platform.common.entity.ResCodeEnum;
import com.bee.platform.common.entity.ResponseResult;

import feign.hystrix.FallbackFactory;
import org.springframework.stereotype.Component;

@Component
public class SiBeeCloudManufactureBeatFeignClientFallbackFactory implements FallbackFactory<SiBeeCloudManufactureBeatFeignClient> {

	@Override
	public SiBeeCloudManufactureBeatFeignClient create(Throwable cause) {

		return new SiBeeCloudManufactureBeatFeignClient() {

			@Override
			public ResponseResult<String> beat() {
				return ResponseResult.buildResponseResult(ResCodeEnum.HYSTRIX_ENABLED, ResCodeEnum.HYSTRIX_ENABLED.msg);
			}

		};
	}

}
