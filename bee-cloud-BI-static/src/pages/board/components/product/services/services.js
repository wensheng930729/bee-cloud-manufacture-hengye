import apis from './api'
import request from '@/utils/request'

export async function getRecoveryRate(params) {
  return request(apis.getRecoveryRate.api(), {
    method: apis.getRecoveryRate.type,
    body: params
  })
}

export async function getPowerFactor(params) {
  return request(apis.getPowerFactor.api(), {
    method: apis.getPowerFactor.type
  })
}

export async function getProductSpec(params) {
  return request(apis.getProductSpec.api(), {
    method: apis.getProductSpec.type,
    body: params
  })
}

export async function getProduction(params) {
  return request(apis.getProduction.api(), {
    method: apis.getProduction.type,
    body: params
  })
}

export async function getPowerConsume(params) {
  return request(apis.getPowerConsume.api(), {
    method: apis.getPowerConsume.type,
    body: params
  })
}

export async function getPowerTonConsume(params) {
  return request(apis.getPowerTonConsume.api(), {
    method: apis.getPowerTonConsume.type,
    body: params
  })
}

export async function getMaterialConsume(params) {
  return request(apis.getMaterialConsume.api(), {
    method: apis.getMaterialConsume.type,
    body: params
  })
}

export async function getMaterialTonConsume(params) {
  return request(apis.getMaterialTonConsume.api(), {
    method: apis.getMaterialTonConsume.type,
    body: params
  })
}

export async function getDeviceInspection() {
  return request(apis.getDeviceInspection.api(), {
    method: apis.getDeviceInspection.type
  })
}