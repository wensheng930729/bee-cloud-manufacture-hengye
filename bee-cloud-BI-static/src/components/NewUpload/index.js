import { Component } from "react";
import PropTypes from "prop-types";
import { Icon, Modal, Upload, message, Progress } from 'antd';
import styles from "./index.less";

export default class NewUpload extends Component {
  state = {
    previewVisible: false,
    previewImage: null,
  };

  handlePreview = file => {
    this.setState({
      previewImage: file.url || file.thumbUrl,
      previewVisible: true,
    });
  };

  render() {
    const { accept, disabled, fileList, number, percent, beforeUpload, onChange, onRemove } = this.props;
    const { previewVisible, previewImage } = this.state;
    const file = fileList || [];
    const uploadButton = (
      <div className={styles.upload}>
        <Icon style={{ fontSize: 20 }} type="plus" />
        <div>上传</div>
      </div>
    );
    // console.log(file, number)
    // console.log(file.length, number)
    return (
      <div className="clearfix">
        <Upload
          accept={accept}
          action={`https://www.beesrv.com/bee-web/api/files/uploadFile`}
          className={styles.imgList}
          disabled={disabled}
          fileList={file}
          listType="picture-card"
          beforeUpload={beforeUpload}
          onChange={onChange}
          onRemove={disabled === true ? () => false : onRemove}
          onPreview={this.handlePreview}
        >
          {
            file.length < number && disabled === false ? uploadButton : null
          }
        </Upload>
        {
          percent && percent !== 0 ? <Progress
            style={{ position: 'relative', top: -8 }}
            percent={percent}
            size="small"
            status={percent === 100 ? 'success' : percent === 101 ? 'exception' : 'active'}
          /> : null
        }
        <Modal visible={previewVisible} footer={null} onCancel={() => this.setState({ previewImage: null, previewVisible: false })}>
          <img alt="example" style={{ width: '100%' }} src={previewImage} />
        </Modal>
      </div>
    );
  }
}

//限定控件传入的属性类型
NewUpload.propTypes = {
  accept: PropTypes.string, // 上传文件格式
  disabled: PropTypes.bool, // 是否禁用
  fileList: PropTypes.array, // 文件列表
  number: PropTypes.number, // 上传文件数
  percent: PropTypes.number, // 上传进度百分比
  beforeUpload: PropTypes.func, // 上传之前钩子函数
  onChange: PropTypes.func,  // 选中时的回调
  onRemove: PropTypes.func, // 移除文件回调
};

//设置默认属性
NewUpload.defaultProps = {
  accept: 'image/jpg, image/jpeg, image/png', //默认接受图片文件
  disabled: false, // 默认不禁用
  fileList: null, //文件列表
  number: 1, //默认最多上传一张图
  percent: 0, // 0-未开始上传   1-99上传中   100-上传成功   101-上传异常
  beforeUpload: (file, fileList) => {
    const isMax10M = file.size / 1024 / 1024 < 10; // 最大10M
    if (!isMax10M) {
      message.error("文件大小最大支持10M！");
    }
    return isMax10M;
  },
  onChange: () => false,
  onRemove: () => true,
};