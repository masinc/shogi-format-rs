// http://www2.computer-shogi.org/protocol/record_v22.html

pub struct Csa {
    version: String,
    metadata: CsaMetadata,
}

pub struct CsaMetadata {
    black_player: Option<String>,
    white_player: Option<String>,
    event: Option<String>,
    site: Option<String>,
    start_time: Option<String>,
    end_time: Option<String>,
    // TODO: 秒読みと持ち時間をわける
    time_limit: Option<String>,
    opening: Option<String>,
}

pub struct CsaMoves {}
